open Outcome_test_support

let installed_binary environment name =
  Filename.concat (Project_environment.binary_root environment) name

let expectation facts =
  List.fold_left
    (fun expectation fact -> Expectation.require_process_fact fact expectation)
    Expectation.empty facts

let run_threads arguments ~environment ~workspace =
  Process_adapter.run ~cwd:workspace
    { program = installed_binary environment "verocaml";
      arguments = "verify" :: "/definitely/missing.cmt" :: arguments;
      forwarded = [ ("OCAML_COLOR", "never") ];
      cleanup_paths = [];
      adjacency = [] }

let rejected name arguments =
  Suite.case ~name
    ~expectation:
      (expectation
         [ Outcome.Exit_class (Outcome.Exited 2);
           Outcome.Stable_code "VERO_CLI";
           Outcome.Forwarded "OCAML_COLOR" ])
    (run_threads arguments)

let () =
  Suite.run_cli ~suite_path:"test/prepared_vc_jobs/outcome_process_cases.ml"
    ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ rejected "threads-zero" [ "--threads"; "0" ];
      rejected "threads-negative" [ "--threads"; "-1" ];
      rejected "threads-nonnumeric" [ "--threads"; "nope" ];
      rejected "threads-overflow" [ "--threads"; "999999999999999999999999999" ];
      rejected "threads-duplicate"
        [ "--threads"; "1"; "--threads"; "2" ];
      rejected "threads-missing-value" [ "--threads" ];
      rejected "threads-runtime-maximum" [ "--threads"; "999999" ] ]
