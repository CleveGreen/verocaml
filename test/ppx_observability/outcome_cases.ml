open Outcome_test_support

type process_result = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let run_process ~workspace ~environment program arguments =
  let stdout_path = Filename.concat workspace "stdout" in
  let stderr_path = Filename.concat workspace "stderr" in
  let stdout_fd = Unix.openfile stdout_path [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  let stderr_fd = Unix.openfile stderr_path [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  let argv = Array.of_list (program :: arguments) in
  let pid = Unix.create_process_env program argv environment Unix.stdin stdout_fd stderr_fd in
  Unix.close stdout_fd;
  Unix.close stderr_fd;
  let _, status = Unix.waitpid [] pid in
  { status; stdout = read_file stdout_path; stderr = read_file stderr_path }

let require condition message =
  if condition then Ok ()
  else Error (Failure.make Failure.Expectation_mismatch message)

let ( let* ) = Result.bind

let executable_directory () =
  let executable =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  Filename.dirname executable

let fixture name =
  Filename.concat (executable_directory ()) (Filename.concat "fixtures" name)
  |> read_file

let find_program path name =
  match
    String.split_on_char ':' path
    |> List.map (fun directory -> Filename.concat directory name)
    |> List.find_opt Sys.file_exists
  with
  | Some program -> program
  | None -> failwith ("program is absent from the authenticated tool path: " ^ name)

let observable_boundaries ~environment ~workspace =
  let implementation = Filename.concat workspace "implementation.ml" in
  let interface = Filename.concat workspace "interface.mli" in
  write_file implementation (fixture "implementation.ml");
  write_file interface (fixture "interface.mli");
  let ocamlc = find_program (Project_environment.tool_path environment) "ocamlc" in
  let ppx = Filename.concat (Project_environment.binary_root environment) "verocaml-ppx" in
  let tool_path = Project_environment.tool_path environment in
  let quiet_environment =
    [| "PATH=" ^ tool_path; "OCAML_COLOR=never" |]
  in
  let traced_environment =
    [|
      "PATH=" ^ tool_path;
      "OCAML_COLOR=never";
      "DELATOR_LOG=trace";
      "DELATOR_FORMAT=flat";
      "DELATOR_COLOR=never";
    |]
  in
  let quiet =
    run_process ~workspace ~environment:quiet_environment ocamlc
      [ "-c"; "-ppx"; ppx; "-o"; Filename.concat workspace "quiet.cmo"; implementation ]
  in
  let* () = require (quiet.status = Unix.WEXITED 0) "quiet PPX compilation failed" in
  let* () =
    require (quiet.stdout = "" && quiet.stderr = "")
      "the PPX emitted output while tracing was disabled"
  in
  let traced =
    run_process ~workspace ~environment:traced_environment ocamlc
      [ "-c"; "-ppx"; ppx; "-o"; Filename.concat workspace "traced.cmo"; implementation ]
  in
  let* () = require (traced.status = Unix.WEXITED 0) "traced PPX compilation failed" in
  let* () = require (traced.stderr <> "") "opt-in PPX tracing emitted no events" in
  let traced_interface =
    run_process ~workspace ~environment:traced_environment ocamlc
      [ "-c"; "-ppx"; ppx; "-o"; Filename.concat workspace "traced.cmi"; interface ]
  in
  let* () =
    require (traced_interface.status = Unix.WEXITED 0)
      "traced PPX interface compilation failed"
  in
  let* () =
    require (traced_interface.stderr <> "")
      "opt-in interface tracing emitted no events"
  in
  let public_driver =
    Filename.concat (executable_directory ()) "ppx_driver_observability_tool.exe"
  in
  let quiet_driver =
    run_process ~workspace ~environment:quiet_environment public_driver []
  in
  let* () = require (quiet_driver.status = Unix.WEXITED 0) "public PPX driver failed" in
  let* () =
    require (quiet_driver.stdout = "" && quiet_driver.stderr = "")
      "the public PPX driver emitted output while tracing was disabled"
  in
  let traced_driver =
    run_process ~workspace ~environment:traced_environment public_driver []
  in
  let* () = require (traced_driver.status = Unix.WEXITED 0) "traced public driver failed" in
  let* () = require (traced_driver.stderr <> "") "public driver tracing emitted no events" in
  let semantic_cases =
    List.concat_map
      (fun route ->
        List.concat_map
          (fun mode ->
            List.map
              (fun entrypoint -> (route, mode, entrypoint))
              [ "implementation"; "interface" ])
          [ "ordinary"; "retained" ])
      [ "standalone"; "ppxlib" ]
  in
  let* () =
    List.fold_left
      (fun result (route, mode, entrypoint) ->
        let* () = result in
        let transformed =
          run_process ~workspace ~environment:quiet_environment public_driver
            [ "--semantic"; route; mode; entrypoint ]
        in
        let* () =
          require (transformed.status = Unix.WEXITED 0)
            (Printf.sprintf "%s %s %s semantic PPX check failed" route mode
               entrypoint)
        in
        require (transformed.stdout = "" && transformed.stderr = "")
          (Printf.sprintf "%s %s %s semantic PPX check emitted output" route
             mode entrypoint))
      (Ok ()) semantic_cases
  in
  Ok
    (Outcome.observation ~status:Outcome.Verified
       ~named_facts:
         [
           ("quiet-boundary", Outcome.Function_exists "silent-when-disabled");
           ("implementation-events", Outcome.Function_exists "standalone-implementation");
           ("interface-events", Outcome.Function_exists "standalone-interface");
           ("public-driver-events", Outcome.Function_exists "ppxlib-driver");
           ("runtime-dependency", Outcome.Function_exists "no-delator-reference");
           ("ordinary-module-erasure", Outcome.Function_exists "signature-body-parity");
           ("retained-interface-parity", Outcome.Function_exists "standalone-ppxlib-parity");
         ]
       ()
    |> Outcome.project)

let expectation =
  [
    ("quiet-boundary", "silent-when-disabled");
    ("implementation-events", "standalone-implementation");
    ("interface-events", "standalone-interface");
    ("public-driver-events", "ppxlib-driver");
    ("runtime-dependency", "no-delator-reference");
    ("ordinary-module-erasure", "signature-body-parity");
    ("retained-interface-parity", "standalone-ppxlib-parity");
  ]
  |> List.fold_left
       (fun expectation (name, value) ->
         Expectation.require_named_fact name (Outcome.Function_exists value)
           expectation)
       (Expectation.empty |> Expectation.status Outcome.Verified)

let () =
  Suite.run_cli ~suite_path:"test/ppx_observability/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ Suite.case ~name:"opt-in-observability-boundaries" ~expectation observable_boundaries ]
