open Outcome_test_support

let suite_path = "test/contracts_and_calls/outcome_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let increment_source =
  {|
[@@@verocaml.verify]

let increment (x : int) =
  [%verocaml.requires x < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result = [%verocaml.old x] + 1];
  x + 1
|}

let call_increment_source =
  {|
[@@@verocaml.verify]

let increment (x : int) =
  [%verocaml.requires x < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result = [%verocaml.old x] + 1];
  x + 1

let call_increment (x : int) =
  [%verocaml.requires x < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result -> result > x];
  [%verocaml.assert x < 4_611_686_018_427_387_903];
  increment x
|}

let classify_source =
  {|
[@@@verocaml.verify]

let classify (x : int) =
  [%verocaml.ensures fun result -> result >= 0];
  match x with
  | 0 -> 0
  | n when n > 0 -> 1
  | _ -> 2
|}

let increment_case =
  let input =
    Fixture.single_source ~module_name:"Increment_contract"
      ~source:increment_source ~libraries:[ "verocaml.ghost" ]
  in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Increment_contract" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:increment"
         (Outcome.Function_exists "increment")
    |> Expectation.require_named_fact "obligation-kind:increment"
         (Outcome.Obligation_kind_exists
            {
              function_name = "increment";
              kind = Outcome.Arithmetic_safety (Outcome.Add, Outcome.Lower);
            })
    |> Expectation.require_named_fact "obligation-kind:increment"
         (Outcome.Obligation_kind_exists
            {
              function_name = "increment";
              kind = Outcome.Arithmetic_safety (Outcome.Add, Outcome.Upper);
            })
    |> Expectation.require_named_fact "obligation-kind:increment"
         (Outcome.Obligation_kind_exists
            { function_name = "increment"; kind = Outcome.Postcondition })
  in
  Suite.case ~name:"increment-arithmetic-and-postcondition" ~expectation
    (run_fixture input)

let call_increment_case =
  let input =
    Fixture.single_source ~module_name:"Call_increment_contract"
      ~source:call_increment_source ~libraries:[ "verocaml.ghost" ]
  in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Call_increment_contract" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:call_increment"
         (Outcome.Function_exists "call_increment")
    |> Expectation.require_named_fact "obligation-kind:call_increment"
         (Outcome.Obligation_kind_exists
            { function_name = "call_increment"; kind = Outcome.Assertion })
    |> Expectation.require_named_fact "obligation-kind:call_increment"
         (Outcome.Obligation_kind_exists
            {
              function_name = "call_increment";
              kind = Outcome.Call_precondition { callee = "increment" };
            })
    |> Expectation.require_named_fact "obligation-kind:call_increment"
         (Outcome.Obligation_kind_exists
            { function_name = "call_increment"; kind = Outcome.Postcondition })
  in
  Suite.case ~name:"call-assertion-precondition-and-postcondition" ~expectation
    (run_fixture input)

let classify_case =
  let input =
    Fixture.single_source ~module_name:"Classify_contract"
      ~source:classify_source ~libraries:[ "verocaml.ghost" ]
  in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Verified
    |> Expectation.require_unit "Classify_contract" Outcome.Unit_verified
    |> Expectation.require_named_fact "function:classify"
         (Outcome.Function_exists "classify")
    |> Expectation.require_named_fact "obligation-kind:classify"
         (Outcome.Obligation_kind_exists
            { function_name = "classify"; kind = Outcome.Postcondition })
  in
  Suite.case ~name:"classify-postcondition" ~expectation (run_fixture input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ increment_case; call_increment_case; classify_case ]
