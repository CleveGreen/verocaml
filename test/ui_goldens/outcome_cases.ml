open Outcome_test_support

let executable_directory () =
  let executable =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  Filename.dirname executable

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

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let installed_ghost_directory environment =
  Filename.concat (Project_environment.package_root environment) "verocaml/ghost"

let fixture path = Filename.concat (executable_directory ()) path |> read_file

let run_source source_path ~environment ~workspace =
  let source = Filename.basename source_path in
  write_file (Filename.concat workspace source) (fixture source_path);
  Process_adapter.run ~cwd:workspace
    {
      program = installed_binary environment "verocaml";
      arguments = [ "verify"; source ];
      forwarded =
        [
          ("PATH", Project_environment.tool_path environment);
          ("OCAML_COLOR", "never");
          ("VEROCAML_PPX", installed_binary environment "verocaml-ppx");
          ("VEROCAML_GHOST_DIR", installed_ghost_directory environment);
        ];
      cleanup_paths = [];
      adjacency = [];
    }

let rejected name source_path =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 2))
      |> Expectation.require_process_fact (Outcome.Stable_code "VERO_SOURCE_COMPILE")
      |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR"))
    (run_source source_path)

let verified name source_path =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 0))
      |> Expectation.require_process_fact (Outcome.Forwarded "OCAML_COLOR"))
    (run_source source_path)

let () =
  Suite.run_cli ~suite_path:"test/ui_goldens/outcome_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      rejected "external-specification-ppx-rejection"
        "../external_specifications/fixtures/payload.ml";
      rejected "pure-specification-ppx-rejection"
        "../pure_specifications/fixtures/payload.ml";
      rejected "specification-frontend-ppx-rejection"
        "../specification_frontend/fixtures/invalid_empty.ml";
      rejected "trusted-body-ppx-rejection"
        "../trusted_external_bodies/fixtures/missing_ensures.ml";
      rejected "source-compiler-type-rejection"
        "../source_input/fixtures/compile_error.ml";
      verified "release-source-verifies"
        "../release_verification/fixtures/pure_verified.ml";
    ]
