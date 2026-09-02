open Outcome_test_support

let suite_path = "test/finite_value_receipts/outcome_cases.ml"

let read_file path =
  let path =
    if Sys.file_exists path then path
    else Filename.concat "test/finite_value_receipts" path
  in
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let input name =
  Fixture.single_source ~module_name:(String.capitalize_ascii name)
    ~source:(read_file ("fixtures/" ^ name ^ ".ml"))
    ~libraries:[ "verocaml.ghost"; "verocaml.vstd" ]

let case name fixture expectation =
  Suite.case ~name ~expectation (fun ~environment ~workspace ->
      Fixture.run ~environment ~workspace (input fixture))

let verified unit_name functions =
  List.fold_left
    (fun expectation function_name ->
      Expectation.require_named_fact ("function:" ^ function_name)
        (Outcome.Function_exists function_name) expectation)
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit unit_name Outcome.Unit_verified)
    functions

let rejected unit_name code =
  Expectation.empty |> Expectation.status Outcome.Frontend_rejected
  |> Expectation.require_frontend_code code
  |> Expectation.require_unit unit_name Outcome.Unit_frontend_rejected

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      case "positive-construction-flows" "positive"
        (verified "Positive" [ "verified"; "local_exec_chain"; "local_tracked_chain" ]);
      case "local-result-transfer" "local_result"
        (verified "Local_result" [ "Box.make"; "Box.read"; "run" ]);
      case "direct-cycle-rejection" "direct_cycle"
        (rejected "Direct_cycle" "VERO_UNSUPPORTED_TOP_LEVEL_BINDING");
      case "indirect-cycle-rejection" "indirect_cycle"
        (rejected "Indirect_cycle" "VERO_UNSUPPORTED_TOP_LEVEL_BINDING");
      case "recursive-factory-rejection" "recursive_factory"
        (rejected "Recursive_factory" "VERO_UNSUPPORTED_STRUCTURE_ITEM");
    ]
