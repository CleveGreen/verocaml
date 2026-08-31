open Outcome_test_support

let suite_path = "test/semantic_sst/outcome_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let identity_case =
  let input =
    Fixture.single_source ~module_name:"Semantic_identity"
      ~source:"[@@@verocaml.verify]\nlet identity (value : int) = value\n"
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"identity-validates-and-verifies"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Semantic_identity" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:identity"
           (Outcome.Function_exists "identity"))
    (run_fixture input)

let postcondition_counterexample_case =
  let input =
    Fixture.single_source ~module_name:"Semantic_postcondition_failure"
      ~source:
        {|
[@@@verocaml.verify]
let rejected (value : int) =
  [%verocaml.ensures fun result -> result <> value];
  value
|}
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"postcondition-counterexample"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Semantic_postcondition_failure"
           Outcome.Unit_counterexample
      |> Expectation.require_semantic ~function_name:"rejected"
           Outcome.Postcondition)
    (run_fixture input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ identity_case; postcondition_counterexample_case ]
