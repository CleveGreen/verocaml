open Outcome_test_support

type ecosystem_case = {
  unit_name : string;
  retained : bool;
  open_vstd : bool;
  forwarder : bool;
}

let executable_directory () =
  let executable =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  Filename.dirname executable

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

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

let generate_retained_include ~environment ~workspace ~output_name arguments =
  let output_path = Filename.concat workspace output_name in
  let output = Unix.openfile output_path [ O_WRONLY; O_CREAT; O_EXCL ] 0o600 in
  let program = installed_binary environment "verocaml-retained-interface" in
  let arguments = Array.of_list (program :: "dune-stanza" :: arguments) in
  let status =
    Fun.protect
      ~finally:(fun () -> Unix.close output)
      (fun () ->
        let process =
          Unix.create_process program arguments Unix.stdin output Unix.stderr
        in
        snd (Unix.waitpid [] process))
  in
  match status with
  | Unix.WEXITED 0 -> ()
  | WEXITED code ->
      failwith
        (Printf.sprintf
           "verocaml-retained-interface dune-stanza exited with status %d" code)
  | WSIGNALED signal ->
      failwith
        (Printf.sprintf
           "verocaml-retained-interface dune-stanza was signaled (%d)" signal)
  | WSTOPPED signal ->
      failwith
        (Printf.sprintf
           "verocaml-retained-interface dune-stanza was stopped (%d)" signal)

let project_dune case =
  let libraries =
    if case.forwarder then
      "vstd_broadcast_forwarder vstd verocaml.ghost"
    else "verocaml.vstd verocaml.ghost"
  and open_vstd =
    (if case.open_vstd then " -open Vstd" else "")
    ^
    (if case.retained && not case.forwarder then " -open Vstd.Pervasive"
     else "")
  and preprocess =
    if case.retained then
      "(preprocess (pps verocaml.ppx -- --verocaml-retained))"
    else "(preprocess (pps verocaml.ppx))"
  in
  let forwarder =
    if case.forwarder then
      {|(library
 (name vstd)
 (modules int seq)
 (libraries verocaml.ghost)
 (flags (:standard -w -A -alert -all))
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))

(include vstd__Int.verocaml.inc)
(include vstd__Seq.verocaml.inc)

(library
 (name vstd_broadcast_forwarder)
 (modules vstd_broadcast_forwarder)
 (libraries vstd verocaml.ghost)
 (flags (:standard -w -A -alert -all))
 (preprocess (pps verocaml.ppx -- --verocaml-retained)))

(include vstd_broadcast_forwarder.verocaml.inc)

|}
    else ""
  in
  Printf.sprintf
    {|%s(library
 (name %s)
 (modules %s)
 (libraries %s)
 (flags (:standard -w -A -alert -all%s))
 %s)
|}
    forwarder case.unit_name case.unit_name libraries open_vstd preprocess

let materialize_project case ~environment workspace =
  try
    let source =
      Filename.concat (executable_directory ()) (case.unit_name ^ ".ml")
    in
    write_file (Filename.concat workspace "dune-project")
      (Printf.sprintf
         "(lang dune 3.17)\n(name %s)\n%s" case.unit_name
         (if case.forwarder then
            Printf.sprintf "(package (name %s))\n" case.unit_name
          else ""));
    write_file (Filename.concat workspace "dune") (project_dune case);
    write_file (Filename.concat workspace (case.unit_name ^ ".ml"))
      (read_file source);
    if case.forwarder then (
      let library_directory =
        Filename.concat (executable_directory ()) "../../library" |> Unix.realpath
      in
      write_file (Filename.concat workspace "seq.ml")
        (read_file (Filename.concat library_directory "seq.ml"));
      write_file (Filename.concat workspace "seq.mli")
        (read_file (Filename.concat library_directory "seq.mli"));
      write_file (Filename.concat workspace "int.ml")
        (read_file (Filename.concat library_directory "int.ml"));
      write_file (Filename.concat workspace "int.mli")
        (read_file (Filename.concat library_directory "int.mli"));
      write_file (Filename.concat workspace "vstd_broadcast_forwarder.ml")
        (read_file
           (Filename.concat (executable_directory ())
              "vstd_broadcast_forwarder.ml"));
      write_file (Filename.concat workspace "vstd_broadcast_forwarder.mli")
        (read_file
           (Filename.concat (executable_directory ())
              "vstd_broadcast_forwarder.mli"));
      generate_retained_include ~environment ~workspace
        ~output_name:"vstd__Int.verocaml.inc"
        [
          case.unit_name;
          ".";
          "vstd";
          "Vstd__Int";
          "--transport-unit";
          "Vstd";
        ];
      generate_retained_include ~environment ~workspace
        ~output_name:"vstd__Seq.verocaml.inc"
        [
          case.unit_name;
          ".";
          "vstd";
          "Vstd__Seq";
          "--transport-unit";
          "Vstd";
          ".";
          "vstd";
          "Vstd__Int";
        ];
      generate_retained_include ~environment ~workspace
        ~output_name:"vstd_broadcast_forwarder.verocaml.inc"
        [
          case.unit_name;
          ".";
          "vstd_broadcast_forwarder";
          "Vstd_broadcast_forwarder";
          "--dependency-transport-unit";
          "Vstd";
          ".";
          "vstd";
          "Vstd__Seq";
          ".";
          "vstd";
          "Vstd__Int";
        ]);
    Ok ()
  with
  | Failure message | Sys_error message | Unix.Unix_error (_, _, message) ->
      Error (Failure.make Failure.Project_materialization message)

let with_unit_fact unit_name outcome =
  Outcome.observation ~status:(Outcome.status outcome)
    ~frontend_codes:(Outcome.frontend_codes outcome)
    ~semantic_facts:(Outcome.semantic_facts outcome)
    ~units:(Outcome.units outcome)
    ~named_facts:
      (("verified-directory", Outcome.Function_exists unit_name)
      :: Outcome.named_facts outcome)
    ~process_facts:(Outcome.process_facts outcome) ()
  |> Outcome.project

let verify_project case ~environment ~workspace =
  let workspace = Unix.realpath workspace in
  Result.bind (materialize_project case ~environment workspace) (fun () ->
      Result.map (with_unit_fact case.unit_name)
        (Process_adapter.run ~cwd:workspace
           {
             program = installed_binary environment "verocaml";
             arguments =
               [ "verify"; "."; "--threads"; "2"; "--timeout-ms"; "60000" ];
             forwarded =
               [
                 ("PATH", Project_environment.tool_path environment);
                 ("OCAMLPATH", Project_environment.ocaml_path environment);
                 ("VEROCAML_DUNE", Project_environment.dune_path environment);
                 ("DUNE_CACHE", "disabled");
                 ("HOME", workspace);
                 ("TMPDIR", workspace);
                 ("OCAML_COLOR", "never");
               ];
             cleanup_paths = [];
             adjacency = [];
           }))

let outcome_case case =
  Suite.case
    ~name:
      (String.map (function '_' -> '-' | character -> character) case.unit_name)
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact
           (Outcome.Exit_class (Outcome.Exited 0))
      |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR")
      |> Expectation.require_process_fact (Outcome.Forwarded "OCAMLPATH")
      |> Expectation.require_process_fact (Outcome.Forwarded "VEROCAML_DUNE")
      |> Expectation.require_named_fact "verified-directory"
           (Outcome.Function_exists case.unit_name))
    (verify_project case)

let cases =
  [
    {
      unit_name = "vstd_consumer";
      retained = true;
      open_vstd = false;
      forwarder = false;
    };
    {
      unit_name = "ambient_vstd_consumer";
      retained = false;
      open_vstd = false;
      forwarder = false;
    };
    {
      unit_name = "imported_generic_axiom_consumer";
      retained = true;
      open_vstd = true;
      forwarder = false;
    };
    {
      unit_name = "imported_broadcast_group_consumer";
      retained = true;
      open_vstd = true;
      forwarder = false;
    };
    {
      unit_name = "imported_broadcast_declaration_consumer";
      retained = true;
      open_vstd = true;
      forwarder = false;
    };
    {
      unit_name = "forwarded_broadcast_group_consumer";
      retained = true;
      open_vstd = true;
      forwarder = true;
    };
    {
      unit_name = "selected_broadcast_group_consumer";
      retained = true;
      open_vstd = true;
      forwarder = true;
    };
  ]

let () =
  Suite.run_cli ~suite_path:"test/vstd_integration/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    (List.map outcome_case cases)
