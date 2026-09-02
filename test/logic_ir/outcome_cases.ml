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

let span =
  Diagnostic.
    {
      file = "logic_ir_outcome.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

let nonlinear_query () =
  let ( let* ) = Result.bind in
  let builder = Logic_ir.create () in
  let* left_symbol =
    Logic_ir.declare_function builder ~name:"left" ~domain:[]
      ~range:Logic_ir.Int ~span
  in
  let* right_symbol =
    Logic_ir.declare_function builder ~name:"right" ~domain:[]
      ~range:Logic_ir.Int ~span
  in
  let* left = Logic_ir.apply ~span left_symbol [] in
  let* right = Logic_ir.apply ~span right_symbol [] in
  let* product = Logic_ir.multiply ~span left right in
  let* reverse = Logic_ir.multiply ~span right left in
  let* commutative = Logic_ir.equal ~span product reverse in
  let* counterexample = Logic_ir.not_ ~span commutative in
  Logic_ir.query builder ~axioms:[] ~assertions:[ counterexample ] ~requires:[]
    ~span

let nonlinear_requirement_case =
  Suite.case ~name:"multiplication-propagates-nonlinear-requirement"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      match nonlinear_query () with
      | Ok query
        when List.mem Logic_ir.Nonlinear_integer_arithmetic
               (Logic_ir.requirements query) ->
          Ok
            (Outcome.observation ~status:Outcome.Verified ()
            |> Outcome.project)
      | Ok _ ->
          Error
            (Failure.make Failure.Expectation_mismatch
               "multiplication omitted its nonlinear logic requirement")
      | Error error ->
          Error
            (Failure.make Failure.Runner_internal
               (Logic_ir.error_to_string error)))

let nonlinear_query_case =
  Suite.case ~name:"nonlinear-logic-query-verifies"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      match nonlinear_query () with
      | Error error ->
          Error
            (Failure.make Failure.Runner_internal
               (Logic_ir.error_to_string error))
      | Ok query -> (
          match
            Z3_bridge.solve_query { timeout_ms = 10_000; model = true } query
          with
          | Ok Z3_bridge.Verified ->
              Ok
                (Outcome.observation ~status:Outcome.Verified ()
                |> Outcome.project)
          | Ok _ ->
              Error
                (Failure.make Failure.Expectation_mismatch
                   "nonlinear commutativity query did not verify")
          | Error error ->
              Error
                (Failure.make Failure.Verifier_outcome
                   (Z3_bridge.error_to_string error))))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      verified_logic_case;
      assertion_logic_case;
      nonlinear_requirement_case;
      nonlinear_query_case;
    ]
