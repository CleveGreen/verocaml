open Outcome_test_support

let suite_path = "test/specification_frontend/outcome_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let verified_contract_source =
  {|
[@@@verocaml.verify]

let checked (value : int) =
  [%verocaml.requires
    value >= 0 && value < 4_611_686_018_427_387_903];
  [%verocaml.assert value >= 0];
  [%verocaml.ensures fun result ->
    result = [%verocaml.old value] + 1];
  value + 1
|}

let assertion_counterexample_source =
  {|
[@@@verocaml.verify]

let rejected (value : int) =
  [%verocaml.assert false];
  value
|}

let verified_contract_case =
  let input =
    Fixture.single_source ~module_name:"Verified_contract"
      ~source:verified_contract_source ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"retained-contract-semantics"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Verified_contract" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:checked"
           (Outcome.Function_exists "checked")
      |> Expectation.require_named_fact "obligation-kind:checked"
           (Outcome.Obligation_kind_exists
              { function_name = "checked"; kind = Outcome.Assertion })
      |> Expectation.require_named_fact "obligation-kind:checked"
           (Outcome.Obligation_kind_exists
              { function_name = "checked"; kind = Outcome.Postcondition }))
    (run_fixture input)

let assertion_counterexample_case =
  let input =
    Fixture.single_source ~module_name:"Assertion_counterexample"
      ~source:assertion_counterexample_source ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"retained-assertion-counterexample"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Assertion_counterexample"
           Outcome.Unit_counterexample
      |> Expectation.require_semantic ~function_name:"rejected"
           Outcome.Assertion)
    (run_fixture input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ verified_contract_case; assertion_counterexample_case ]
