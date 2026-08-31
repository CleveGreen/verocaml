open Outcome_test_support

let suite_path = "test/private_receipt/outcome_cases.ml"

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

let fixture name =
  let relative = Filename.concat "fixtures" name in
  let adjacent = Filename.concat (executable_directory ()) relative in
  let path =
    if Sys.file_exists adjacent then adjacent
    else Filename.concat "test/private_receipt" relative
  in
  read_file path

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let semantic_case ~name ~module_name ~filename expectation =
  let input =
    Fixture.single_source ~module_name ~source:(fixture filename)
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name ~expectation (run_fixture input)

let local_positive_case =
  semantic_case ~name:"local-receipt-dependent-verifies"
    ~module_name:"Local_positive" ~filename:"local_positive.ml"
    (Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Local_positive" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:Box.make"
         (Outcome.Function_exists "Box.make")
    |> Expectation.require_named_fact "function:run"
         (Outcome.Function_exists "run"))

let failed_callee_case =
  semantic_case ~name:"failed-callee-blocks-dependent"
    ~module_name:"Failing_callee" ~filename:"failing_callee.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Failing_callee" Outcome.Unit_counterexample
    |> Expectation.require_named_fact "function:Box.make"
         (Outcome.Function_exists "Box.make"))

let independent_failure_case =
  semantic_case ~name:"independent-receipt-source-still-runs"
    ~module_name:"Independent_receipt_failure"
    ~filename:"independent_receipt_failure.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Independent_receipt_failure"
         Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"bad_copy" Outcome.Assertion
    |> Expectation.require_named_fact "function:run_good"
         (Outcome.Function_exists "run_good"))

let false_precondition_case =
  semantic_case ~name:"receipt-does-not-prove-call-precondition"
    ~module_name:"False_precondition" ~filename:"false_precondition.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "False_precondition" Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"run"
         (Outcome.Call_precondition { callee = "Box.make" }))

let branch_laundering_case =
  semantic_case ~name:"branch-results-do-not-launder-invariant"
    ~module_name:"Branch_laundering" ~filename:"branch_laundering.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Branch_laundering" Outcome.Unit_counterexample
    |> Expectation.require_named_fact "function:run"
         (Outcome.Function_exists "run"))

let rebind_laundering_case =
  semantic_case ~name:"rebound-results-do-not-launder-invariant"
    ~module_name:"Rebind_laundering" ~filename:"rebind_laundering.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Rebind_laundering" Outcome.Unit_counterexample
    |> Expectation.require_named_fact "function:run"
         (Outcome.Function_exists "run"))

let unrelated_failure_case =
  semantic_case ~name:"unrelated-failure-preserves-receipt-path"
    ~module_name:"Unrelated_failure" ~filename:"unrelated_failure.ml"
    (Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Unrelated_failure" Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"unrelated" Outcome.Assertion
    |> Expectation.require_named_fact "function:run"
         (Outcome.Function_exists "run"))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ local_positive_case; failed_callee_case; independent_failure_case;
      false_precondition_case; branch_laundering_case; rebind_laundering_case;
      unrelated_failure_case ]
