open Outcome_test_support

let suite_path = "test/solver_backend/outcome_cases.ml"

let span =
  Diagnostic.
    {
      file = "solver_backend_outcome.ml";
      start_pos = { line = 1; column = 0 };
      end_pos = { line = 1; column = 1 };
    }

let symbol id name sort =
  Vir.{ symbol_id = id; source_name = name; sort; role = Input; span }

let integer value = Vir.Integer_constant value
let integer_symbol symbol = Vir.Integer_symbol symbol

let equal left right = Vir.Integer_compare (Equal, left, right)

let obligation ?(assumptions = []) ?(required_preceding_safety = []) ~kind
    goal =
  Vir.
    {
      obligation_index = 0;
      function_ref = { function_index = 0; function_name = "backend_probe" };
      kind;
      span;
      assumptions;
      required_preceding_safety;
      path_condition = [];
      goal;
      projection_symbols = [];
    }

let configuration () =
  match Solver_backend.config ~timeout_ms:10_000 with
  | Ok configuration -> Ok configuration
  | Error error ->
      Error
        (Failure.make Failure.Runner_internal
           (Solver_backend.error_to_string error))

let projection ~semantic_kind outcome =
  let status =
    match outcome with
    | Solver_backend.Verified -> Outcome.Verified
    | Counterexample _ -> Outcome.Counterexample
    | Inconclusive _ -> Outcome.Inconclusive
  in
  let semantic_facts =
    match (status, semantic_kind) with
    | Outcome.Counterexample, Some kind ->
        [ Outcome.{ function_name = "backend_probe"; kind } ]
    | ( Outcome.Verified | Outcome.Inconclusive | Outcome.Incomplete_source
      | Outcome.Frontend_rejected ),
      _
    | Outcome.Counterexample, None ->
        []
  in
  Outcome.observation ~status ~semantic_facts () |> Outcome.project

let solve ?semantic_kind fixture ~environment:_ ~workspace:_ =
  match configuration () with
  | Error _ as error -> error
  | Ok configuration -> (
      match Solver_backend.solve_obligation configuration fixture with
      | Ok outcome -> Ok (projection ~semantic_kind outcome)
      | Error error ->
          Error
            (Failure.make Failure.Verifier_outcome
               (Solver_backend.error_to_string error)))

let verified_case name fixture =
  Suite.case ~name
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (solve fixture)

let counterexample_case name semantic_kind fixture =
  Suite.case ~name
    ~expectation:
      (Expectation.empty |> Expectation.status Outcome.Counterexample
      |> Expectation.require_semantic ~function_name:"backend_probe"
           semantic_kind)
    (solve ~semantic_kind fixture)

let assertion_kind = Vir.Assertion { assertion_ordinal = 0 }

let arbitrary_precision =
  let beyond_host = Z.(add (shift_left one 100) (of_int 7)) in
  let split =
    Vir.Integer_add (integer Z.(shift_left one 100), integer (Z.of_int 7))
  in
  verified_case "arbitrary-precision-arithmetic-verifies"
    (obligation ~kind:assertion_kind (equal (integer beyond_host) split))

let absolute_minimum_overflow =
  let absolute_minimum =
    Vir.Integer_absolute_value (integer Int_bounds.minimum)
  in
  let semantic_kind =
    Outcome.Arithmetic_safety (Outcome.Absolute_value, Outcome.Upper)
  in
  let vir_kind =
    Vir.Arithmetic_safety
      {
        operation = Absolute_value;
        mathematical_result = integer Z.zero;
        violated_bound = Upper_bound;
      }
  in
  counterexample_case "absolute-minimum-overflow-is-counterexample"
    semantic_kind
    (obligation ~kind:vir_kind
       (Vir.Integer_compare
          (Less_or_equal, absolute_minimum, integer Int_bounds.maximum)))

let false_goal =
  counterexample_case "false-goal-is-counterexample" Outcome.Assertion
    (obligation ~kind:assertion_kind (Vir.Boolean_constant false))

let preceding_safety =
  let value = symbol 0 "value" Vir.Integer in
  let term = integer_symbol value in
  let is_zero = equal term (integer Z.zero) in
  let is_one = equal term (integer Z.one) in
  verified_case "preceding-safety-discharges-dependent-goal"
    (obligation ~kind:assertion_kind ~assumptions:[ is_zero ]
       ~required_preceding_safety:[ is_one ] (Vir.Boolean_constant false))

let nonpositive_timeout =
  Suite.case ~name:"nonpositive-timeout-is-rejected"
    ~expectation:(Expectation.empty |> Expectation.status Outcome.Verified)
    (fun ~environment:_ ~workspace:_ ->
      match Solver_backend.config ~timeout_ms:0 with
      | Error _ ->
          Ok
            (Outcome.observation ~status:Outcome.Verified ()
            |> Outcome.project)
      | Ok _ ->
          Error
            (Failure.make Failure.Expectation_mismatch
               "nonpositive solver timeout was accepted"))

let () =
  Suite.run_cli ~suite_path ~manifest:Integration_environment.manifest
    ~expected_environment:Integration_environment.expected
    [
      arbitrary_precision;
      absolute_minimum_overflow;
      false_goal;
      preceding_safety;
      nonpositive_timeout;
    ]
