open Outcome_test_support

let suite_path = "test/checked_integer_vir/outcome_cases.ml"

let run_fixture input ~environment ~workspace =
  Fixture.run ~environment ~workspace input

let guarded_add_source =
  {|
[@@@verocaml.verify]
let guarded_add (value : int) =
  if value < 4_611_686_018_427_387_903 then value + 1 else value
|}

let guarded_add_case =
  let input =
    Fixture.single_source ~module_name:"Guarded_add" ~source:guarded_add_source
      ~libraries:[ "verocaml.ghost" ]
  in
  Suite.case ~name:"guarded-add-verifies"
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Verified
      |> Expectation.require_unit "Guarded_add" Outcome.Unit_verified
      |> Expectation.require_named_fact "function:guarded_add"
           (Outcome.Function_exists "guarded_add")
      |> Expectation.require_named_fact "obligation-kind:guarded_add"
           (Outcome.Obligation_kind_exists
              {
                function_name = "guarded_add";
                kind = Outcome.Arithmetic_safety (Outcome.Add, Outcome.Lower);
              })
      |> Expectation.require_named_fact "obligation-kind:guarded_add"
           (Outcome.Obligation_kind_exists
              {
                function_name = "guarded_add";
                kind = Outcome.Arithmetic_safety (Outcome.Add, Outcome.Upper);
              }))
    (run_fixture input)

let arithmetic_source =
  {|
[@@@verocaml.verify]
let unguarded_add (value : int) = value + 1
let unguarded_subtract (value : int) = value - 1
let negate (value : int) = -value
let scale (value : int) = value * 3
let standard (value : int) = succ value + pred (abs value)
|}

let arithmetic_counterexamples_case =
  let input =
    Fixture.single_source ~module_name:"Arithmetic_counterexamples"
      ~source:arithmetic_source ~libraries:[ "verocaml.ghost" ]
  in
  let obligation function_name kind expectation =
    Expectation.require_named_fact ("obligation-kind:" ^ function_name)
      (Outcome.Obligation_kind_exists { function_name; kind }) expectation
  in
  let expectation =
    Expectation.empty |> Expectation.status Outcome.Counterexample
    |> Expectation.require_unit "Arithmetic_counterexamples"
         Outcome.Unit_counterexample
    |> Expectation.require_semantic ~function_name:"unguarded_add"
         (Outcome.Arithmetic_safety (Outcome.Add, Outcome.Upper))
    |> Expectation.require_semantic ~function_name:"unguarded_subtract"
         (Outcome.Arithmetic_safety (Outcome.Subtract, Outcome.Lower))
    |> obligation "negate"
         (Outcome.Arithmetic_safety (Outcome.Negate, Outcome.Upper))
    |> obligation "scale"
         (Outcome.Arithmetic_safety (Outcome.Multiply_constant, Outcome.Lower))
    |> obligation "scale"
         (Outcome.Arithmetic_safety (Outcome.Multiply_constant, Outcome.Upper))
    |> obligation "standard"
         (Outcome.Arithmetic_safety (Outcome.Successor, Outcome.Lower))
    |> obligation "standard"
         (Outcome.Arithmetic_safety (Outcome.Predecessor, Outcome.Upper))
    |> obligation "standard"
         (Outcome.Arithmetic_safety (Outcome.Absolute_value, Outcome.Upper))
  in
  Suite.case ~name:"checked-operation-kinds-and-counterexamples" ~expectation
    (run_fixture input)

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [ guarded_add_case; arithmetic_counterexamples_case ]
