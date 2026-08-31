open Outcome_test_support

let suite_path = "test/logic_ir/outcome_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let verified_logic_case =
  let input =
    Fixture.single_source ~module_name:"Logic_identity"
      ~source:"[@@@verocaml.verify]\nlet identity (value : int) = value\n"
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"typed-identity-verifies"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Logic_identity" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:identity"
           (Outcome.Function_exists "identity"))
    (run_fixture input)

let assertion_logic_case =
  let input =
    Fixture.single_source ~module_name:"Logic_assertion_failure"
      ~source:
        "[@@@verocaml.verify]\nlet rejected value =\n  \
         [%verocaml.assert false];\n  value\n"
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"assertion-counterexample"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_unit "Logic_assertion_failure"
           Outcome.Unit_counterexample
      |> Expectation.require_semantic ~function_name:"rejected"
           Outcome.Assertion)
    (run_fixture input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ verified_logic_case; assertion_logic_case ]
