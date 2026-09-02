open Outcome_test_support

let ( let* ) = Result.bind

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

let fixture path = Filename.concat (executable_directory ()) path |> read_file

let rec files_below root =
  if not (Sys.file_exists root) then []
  else if Sys.is_directory root then
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> files_below (Filename.concat root name))
  else [ root ]

let discover_cmt root module_name =
  let expected = String.uncapitalize_ascii module_name ^ ".cmt" in
  match
    files_below root
    |> List.filter (fun path -> String.equal (Filename.basename path) expected)
  with
  | [ path ] -> Ok (Unix.realpath path)
  | _ ->
      Error
        (Failure.make Failure.Selected_cmt_load
           ("prepared fixture CMT is missing or ambiguous: " ^ module_name))

let run_tool arguments ~workspace =
  Process_adapter.run ~cwd:workspace
    {
      program = Filename.concat (executable_directory ()) "prepared_vc_jobs_tool.exe";
      arguments;
      forwarded = [];
      cleanup_paths = [];
      adjacency = [];
    }

let direct_tool_case name mode =
  Suite.case ~name
    ~expectation:
      (Expectation.empty
      |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 0)))
    (fun ~environment:_ ~workspace -> run_tool [ mode ] ~workspace)

let fixture_tool_case ~libraries ~name ~mode ~module_name ~fixture_path =
  Suite.case ~name
    ~expectation:
      (Expectation.empty
      |> Expectation.require_process_fact (Outcome.Exit_class (Outcome.Exited 0)))
    (fun ~environment ~workspace ->
      let build_workspace = Filename.concat workspace "fixture" in
      let input =
        Fixture.single_source ~module_name ~source:(fixture fixture_path)
          ~libraries
      in
      let* _ = Fixture.run ~environment ~workspace:build_workspace input in
      let* cmt =
        discover_cmt (Filename.concat build_workspace "project/_build") module_name
      in
      run_tool [ mode; cmt ] ~workspace)

let () =
  Suite.run_cli ~suite_path:"test/prepared_vc_jobs/outcome_cases.ml"
    ~manifest:Semantic_integration_environment.manifest
    ~expected_environment:Semantic_integration_environment.expected
    [
      direct_tool_case "local-bridge-attempt-isolation" "local-bridge";
      direct_tool_case "prepared-job-ownership-and-accounting" "prepared-job";
      direct_tool_case "resource-exhaustion-cleans-local-state" "resource";
      fixture_tool_case ~name:"recursive-local-routing-and-cleanup"
        ~libraries:[ "verocaml.ghost" ]
        ~mode:"recursive-local" ~module_name:"Nullary_recursive_symbolic_positive"
        ~fixture_path:
          "../proof_body_assertions/fixtures/nullary_recursive_symbolic_positive.ml";
      fixture_tool_case ~name:"production-recursive-commit-order"
        ~libraries:[ "verocaml.ghost" ]
        ~mode:"production-recursive"
        ~module_name:"Nullary_recursive_symbolic_positive"
        ~fixture_path:
          "../proof_body_assertions/fixtures/nullary_recursive_symbolic_positive.ml";
      fixture_tool_case ~name:"production-coordinator-canonical-cutoff"
        ~libraries:[ "verocaml.ghost" ]
        ~mode:"production-coordinator"
        ~module_name:"Builtin_assert_positive_matrix"
        ~fixture_path:
          "../proof_body_assertions/fixtures/builtin_assert_positive_matrix.ml";
      fixture_tool_case ~name:"failed-producer-schedules-no-dependent-work"
        ~libraries:[ "verocaml.ghost" ]
        ~mode:"production-failed-producer" ~module_name:"Failing_callee"
        ~fixture_path:"../private_receipt/fixtures/failing_callee.ml";
    ]
