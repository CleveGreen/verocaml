let () = ignore Solver_backend_prerequisites.ready

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 3)
    format

let assert_true label condition = if not condition then fail "%s" label

let assert_equal_int label expected actual =
  if expected <> actual then
    fail "%s: expected %d, found %d" label expected actual

let span =
  let position line column = Diagnostic.{ line; column } in
  Diagnostic.
    {
      file = "solver_backend_unit.ml";
      start_pos = position 1 0;
      end_pos = position 1 1;
    }

let symbol ?(role = Vir.Input) id name sort =
  Vir.{ symbol_id = id; source_name = name; sort; role; span }

let integer_symbol symbol = Vir.Integer_symbol symbol
let boolean_symbol symbol = Vir.Boolean_symbol symbol
let integer value = Vir.Integer_constant value

let compare comparison left right =
  Vir.Integer_compare (comparison, left, right)

let equal left right = compare Vir.Equal left right

let normalize_whitespace value =
  let buffer = Buffer.create (String.length value) in
  let pending_space = ref false in
  String.iter
    (fun character ->
      match character with
      | ' ' | '\n' | '\r' | '\t' -> pending_space := true
      | character ->
          if !pending_space && Buffer.length buffer > 0 then
            Buffer.add_char buffer ' ';
          pending_space := false;
          Buffer.add_char buffer character)
    value;
  Buffer.contents buffer

let count_substring ~needle value =
  let needle_length = String.length needle in
  let value_length = String.length value in
  let rec loop offset count =
    if offset + needle_length > value_length then count
    else if String.sub value offset needle_length = needle then
      loop (offset + needle_length) (count + 1)
    else loop (offset + 1) count
  in
  loop 0 0

let obligation ?(index = 0) ?(function_index = 0) ?(assumptions = [])
    ?(required_preceding_safety = []) ?(path_condition = [])
    ?(projection_symbols = []) goal =
  let mathematical_result = integer Z.zero in
  Vir.
    {
      obligation_index = index;
      function_ref =
        {
          function_index;
          function_name = Printf.sprintf "function_%d" function_index;
        };
      kind =
        Arithmetic_safety
          {
            operation = Add;
            mathematical_result;
            violated_bound = Upper_bound;
          };
      span;
      assumptions;
      required_preceding_safety;
      path_condition;
      goal;
      projection_symbols;
      logical_constant_instances = [];
      logical_constant_equations = [];
    }

let configured timeout_ms =
  match Solver_backend.config ~timeout_ms with
  | Ok config -> config
  | Error error -> fail "%s" (Solver_backend.error_to_string error)

let solve config obligation =
  match Solver_backend.solve_obligation config obligation with
  | Ok outcome -> outcome
  | Error error -> fail "%s" (Solver_backend.error_to_string error)

let solve_direct ?(requires = []) config obligation =
  let direct_config : Z3_bridge.config =
    { timeout_ms = Solver_backend.timeout_ms config; model = true }
  in
  match Z3_bridge.solve_vir ~requires direct_config obligation with
  | Ok outcome -> outcome
  | Error _ -> fail "direct-Z3 bridge unexpectedly failed"

let expect_verified label = function
  | Solver_backend.Verified -> ()
  | Counterexample _ -> fail "%s: expected verified, found counterexample" label
  | Inconclusive _ -> fail "%s: expected verified, found inconclusive" label

let expect_counterexample label = function
  | Solver_backend.Counterexample model -> model
  | Verified -> fail "%s: expected counterexample, found verified" label
  | Inconclusive _ -> fail "%s: expected counterexample, found inconclusive" label

let expect_integer_binding label expected
    (binding : Solver_backend.model_binding) =
  match binding.value with
  | Some (Solver_backend.Integer actual) when Z.equal expected actual -> ()
  | Some (Integer actual) ->
      fail "%s: expected %s, found %s" label (Z.to_string expected)
        (Z.to_string actual)
  | Some (Boolean _) -> fail "%s: expected integer, found Boolean" label
  | Some (Aggregate_identity _) ->
      fail "%s: expected integer, found aggregate identity" label
  | None -> fail "%s: model omitted value" label

let equal_model_value left right =
  match (left, right) with
  | None, None -> true
  | Some (Solver_backend.Integer left), Some (Z3_bridge.Integer right)
  | Some (Solver_backend.Aggregate_identity left),
    Some (Z3_bridge.Aggregate_identity right) ->
      Z.equal left right
  | Some (Solver_backend.Boolean left), Some (Z3_bridge.Boolean right) ->
      Bool.equal left right
  | (None | Some _), (None | Some _) -> false

let equal_model_binding
    (left : Solver_backend.model_binding)
    (right : Z3_bridge.model_binding) =
  left.symbol = right.symbol && equal_model_value left.value right.value

let assert_outcome_parity label ordinary direct =
  match (ordinary, direct) with
  | Solver_backend.Verified, Z3_bridge.Verified -> ()
  | Solver_backend.Counterexample left, Z3_bridge.Counterexample right ->
      if
        List.length left <> List.length right
        || not (List.for_all2 equal_model_binding left right)
      then fail "%s: counterexample projection mismatch" label
  | Solver_backend.Inconclusive _, Z3_bridge.Inconclusive _ ->
      ()
  | _ -> fail "%s: outcome classification mismatch" label

type expected_projection =
  | Expected_integer of Z.t
  | Expected_boolean of bool
  | Expected_aggregate_identity of Z.t

let assert_exact_projection_parity config label expected_symbol expected
    fixture =
  let ordinary = solve config fixture in
  let direct = solve_direct config fixture in
  assert_outcome_parity label ordinary direct;
  match (ordinary, direct) with
  | ( Solver_backend.Counterexample [ ordinary_binding ],
      Z3_bridge.Counterexample [ direct_binding ] ) ->
      assert_true (label ^ ": ordinary projection symbol changed")
        (ordinary_binding.symbol = expected_symbol);
      assert_true (label ^ ": direct projection symbol changed")
        (direct_binding.symbol = expected_symbol);
      assert_true (label ^ ": normalized bindings differ")
        (equal_model_binding ordinary_binding direct_binding);
      let exact =
        match (expected, ordinary_binding.value) with
        | Expected_integer expected, Some (Solver_backend.Integer actual)
        | ( Expected_aggregate_identity expected,
            Some (Solver_backend.Aggregate_identity actual) ) ->
            Z.equal expected actual
        | Expected_boolean expected, Some (Solver_backend.Boolean actual) ->
            Bool.equal expected actual
        | (Expected_integer _ | Expected_boolean _
          | Expected_aggregate_identity _),
          (None | Some _) ->
            false
      in
      assert_true (label ^ ": ordinary projection value changed") exact
  | Solver_backend.Counterexample _, Z3_bridge.Counterexample _ ->
      fail "%s: expected exactly one normalized binding from each backend" label
  | _ -> fail "%s: expected a counterexample from each backend" label

let solve_parity config obligation =
  let ordinary = solve config obligation in
  let direct = solve_direct config obligation in
  assert_outcome_parity "shared solver fixture" ordinary direct;
  ordinary

let test_raw_large_integers config =
  let beyond_host = Z.(add (shift_left one 100) (of_int 7)) in
  let split =
    Vir.Integer_add
      (integer Z.(shift_left one 100), integer (Z.of_int 7))
  in
  solve_parity config (obligation (equal (integer beyond_host) split))
  |> expect_verified "arbitrary-precision constant";
  let minimum_from_decimal = Z.of_string "-4611686018427387904" in
  let maximum_from_decimal = Z.of_string "4611686018427387903" in
  solve_parity config
    (obligation
       (equal (integer Int_bounds.minimum) (integer minimum_from_decimal)))
  |> expect_verified "minimum bound";
  solve_parity config
    (obligation
       (equal (integer Int_bounds.maximum) (integer maximum_from_decimal)))
  |> expect_verified "maximum bound";
  let coefficient = Z.(add (shift_left one 100) one) in
  let product =
    Vir.Integer_multiply_constant (coefficient, integer (Z.of_int 3))
  in
  solve_parity config
    (obligation
       (equal product (integer Z.(mul coefficient (of_int 3)))))
  |> expect_verified "arbitrary-precision literal coefficient";
  let rendered =
    match
      Solver_backend.For_testing.translated_integer_term ~function_index:0
        (integer beyond_host)
    with
    | Ok rendered -> rendered
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  assert_true "large integer unexpectedly narrowed to a host literal"
    (not (String.equal rendered (Z.to_string beyond_host)));
  assert_true "large integer was not built by raw binary doubling"
    (String.starts_with ~prefix:"(int.add" rendered);
  let x = symbol 11 "symbolic_scale" Vir.Integer in
  let x_term = integer_symbol x in
  let assert_symbolic_scaling coefficient =
    let scaled = Vir.Integer_multiply_constant (coefficient, x_term) in
    solve_parity config
      (obligation
         ~assumptions:[ equal x_term (integer (Z.of_int 3)) ]
         (equal scaled (integer Z.(mul coefficient (of_int 3)))))
    |> expect_verified "arbitrary symbolic literal coefficient";
    let rendered =
      match
        Solver_backend.For_testing.translated_integer_term ~function_index:0
          scaled
      with
      | Ok rendered -> normalize_whitespace rendered
      | Error error -> fail "%s" (Solver_backend.error_to_string error)
    in
    let all_multiplications =
      count_substring ~needle:"(int.mul" rendered
    in
    let direct_literal_doublings =
      count_substring ~needle:"(int.mul 2 " rendered
    in
    assert_equal_int
      "symbolic coefficient emitted a compound/nonliteral multiplier"
      all_multiplications direct_literal_doublings;
    assert_equal_int "symbolic coefficient doubling count changed"
      (Z.numbits (Z.abs coefficient))
      direct_literal_doublings;
    rendered
  in
  let positive = assert_symbolic_scaling coefficient in
  let negative = assert_symbolic_scaling (Z.neg coefficient) in
  assert_true "positive symbolic scaling gained a leading negation"
    (not (String.starts_with ~prefix:"(int.neg" positive));
  assert_true "negative symbolic scaling omitted its one outer negation"
    (String.starts_with ~prefix:"(int.neg" negative)

let test_abs_minimum_overflow config =
  let absolute_minimum =
    Vir.Integer_absolute_value (integer Int_bounds.minimum)
  in
  solve_parity config
    (obligation
       (compare Vir.Less_or_equal absolute_minimum
          (integer Int_bounds.maximum)))
  |> expect_counterexample "abs(min_int) upper bound"
  |> ignore

let test_outcomes_and_projection config =
  solve_parity config (obligation (Vir.Boolean_constant true))
  |> expect_verified "true goal";
  solve_parity config (obligation (Vir.Boolean_constant false))
  |> expect_counterexample "false goal"
  |> ignore;
  let x = symbol 1 "x" Vir.Integer in
  let y = symbol 2 "condition" Vir.Boolean in
  let hidden = symbol 3 "not_projected" Vir.Integer in
  let translated_name function_index =
    match
      Solver_backend.For_testing.translated_integer_term ~function_index
        (integer_symbol x)
    with
    | Ok name -> name
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  assert_true "SMT symbol naming omitted the function index or symbol id"
    (String.equal (translated_name 7) "f7_s1"
    && String.equal (translated_name 8) "f8_s1");
  let assumptions =
    [
      equal (integer_symbol x) (integer (Z.of_int 42));
      equal (integer_symbol hidden) (integer (Z.of_int 9));
      Vir.Boolean_equal
        (boolean_symbol y, Vir.Boolean_constant true);
    ]
  in
  let model =
    solve_parity config
      (obligation ~function_index:7 ~assumptions
         ~projection_symbols:[ y; x; x ]
         (Vir.Boolean_constant false))
    |> expect_counterexample "projected model"
  in
  assert_equal_int "normalized model size" 2 (List.length model);
  let first, second =
    match model with
    | [ first; second ] -> (first, second)
    | _ -> fail "unexpected projected model shape"
  in
  assert_true "projection did not use normalized VIR symbol order"
    (first.symbol.symbol_id = x.symbol_id
    && second.symbol.symbol_id = y.symbol_id);
  assert_true "projection leaked an unrequested model binding"
    (List.for_all
       (fun (binding : Solver_backend.model_binding) ->
         binding.symbol.symbol_id <> hidden.symbol_id)
       model);
  expect_integer_binding "projected x" (Z.of_int 42) first;
  match second.value with
  | Some (Solver_backend.Boolean true) -> ()
  | Some (Boolean false) -> fail "projected Boolean has wrong value"
  | Some (Integer _) -> fail "projected Boolean has integer value"
  | Some (Aggregate_identity _) ->
      fail "projected Boolean has aggregate identity value"
  | None -> fail "projected Boolean value was omitted"

let test_preceding_safety_and_order config =
  let x = symbol 0 "x" Vir.Integer in
  let x_term = integer_symbol x in
  let x_is_zero = equal x_term (integer Z.zero) in
  let x_is_one = equal x_term (integer Z.one) in
  solve_parity config
    (obligation ~assumptions:[ x_is_zero ]
       ~required_preceding_safety:[ x_is_one ]
       (Vir.Boolean_constant false))
  |> expect_verified "explicit preceding-safety dependency";
  solve_parity config
    (obligation ~assumptions:[ x_is_zero ] ~path_condition:[ x_is_one ]
       (Vir.Boolean_constant false))
  |> expect_verified "path-condition dependency";
  let first =
    obligation ~index:0 ~assumptions:[ x_is_zero ] ~projection_symbols:[ x ]
      (compare Vir.Greater_than x_term (integer Z.zero))
  in
  let second =
    obligation ~index:1 ~assumptions:[ x_is_zero ]
      ~required_preceding_safety:[ first.goal ]
      (Vir.Boolean_constant false)
  in
  match Solver_backend.solve_in_order config [ first; second ] with
  | Error error -> fail "%s" (Solver_backend.error_to_string error)
  | Ok [ { obligation; outcome = Counterexample _ } ] ->
      assert_true "ordered solving returned the wrong failed obligation"
        (obligation.obligation_index = 0)
  | Ok _ ->
      fail
        "ordered solving continued after failure and allowed a conditional VC \
         to mask it"

let test_unknown_errors_and_cleanup config =
  let trivial = obligation (Vir.Boolean_constant true) in
  let before = Solver_backend.For_testing.reset_count () in
  (match
     Solver_backend.For_testing.solve_after_translation Unknown config trivial
   with
  | Ok
      (Solver_backend.Inconclusive
        {
          configured_timeout_ms;
          configured_rlimit;
          reason = Backend_unknown "controlled unknown";
        })
    when
      configured_timeout_ms = 60000
      && configured_rlimit = Solver_backend.rlimit config ->
      ()
  | Ok _ -> fail "controlled unknown was not classified as inconclusive"
  | Error error -> fail "%s" (Solver_backend.error_to_string error));
  assert_equal_int "reset after controlled unknown" (before + 1)
    (Solver_backend.For_testing.reset_count ());
  (match
     Solver_backend.For_testing.solve_after_translation Backend_failure config
       trivial
   with
  | Error (Solver_backend.Backend_failure _) -> ()
  | Error error ->
      fail "controlled backend failure misclassified: %s"
        (Solver_backend.error_to_string error)
  | Ok _ -> fail "controlled backend failure was accepted");
  assert_equal_int "reset after controlled backend failure" (before + 2)
    (Solver_backend.For_testing.reset_count ());
  let wrong_sort = symbol 9 "wrong_sort" Vir.Integer in
  let creations_before_translation_failure =
    Solver_backend.For_testing.solver_creation_count ()
  in
  (match
     Solver_backend.solve_obligation config
       (obligation (Vir.Boolean_symbol wrong_sort))
   with
  | Error (Solver_backend.Malformed_vir _) -> ()
  | Error error ->
      fail "malformed VIR was misclassified: %s"
        (Solver_backend.error_to_string error)
  | Ok _ -> fail "malformed VIR was accepted");
  assert_equal_int "pre-backend translation failure created a solver"
    creations_before_translation_failure
    (Solver_backend.For_testing.solver_creation_count ());
  assert_equal_int "pre-backend translation failure reset a nonexistent solver"
    (before + 2) (Solver_backend.For_testing.reset_count ())

let aggregate_type index name : Vir.aggregate_type =
  { aggregate_type_index = index; aggregate_type_name = name ; aggregate_type_arguments = []}

let aggregate_symbol type_ symbol : Vir.aggregate_term =
  {
    aggregate_type = type_;
    aggregate_desc = Vir.Aggregate_symbol symbol;
  }

let selector domain range index name path : Vir.selector =
  {
    selector_domain = domain;
    selector_range = range;
    selector_namespace = domain.aggregate_type_name;
    selector_index = index;
    selector_name = name;
    selector_path = path;
  }

let translated_query ~requires obligation =
  match Vir_logic_ir_translation_private.translate ~requires obligation with
  | Ok translated ->
      Vir_logic_ir_translation_private.query translated
  | Error message -> fail "%s" message

let aggregate_symbol_range query =
  match
    Logic_ir.View.declarations query
    |> List.find_map (function
         | Logic_ir.View.Function_declaration function_
           when String.equal
                  (Logic_ir.View.function_name function_)
                  "f0_s42" ->
             Some (Logic_ir.View.function_range function_)
         | Sort_declaration _ | Function_declaration _ -> None)
  with
  | Some range -> range
  | None -> fail "aggregate symbol declaration is missing"

let test_aggregate_sort_policy record =
  let fixture =
    obligation (Vir.Aggregate_equal (record, record))
  in
  let ordinary = translated_query ~requires:[] fixture in
  let named =
    translated_query ~requires:[ Logic_ir.Named_sorts ] fixture
  in
  assert_true "ordinary aggregate did not retain integer representation"
    (aggregate_symbol_range ordinary = Logic_ir.Int);
  let named_sorts =
    Logic_ir.View.declarations named
    |> List.filter_map (function
         | Logic_ir.View.Sort_declaration sort -> Some sort
         | Function_declaration _ -> None)
  in
  assert_true "ordinary aggregate unexpectedly declared a named sort"
    (Logic_ir.View.declarations ordinary
    |> List.for_all (function
         | Logic_ir.View.Function_declaration _ -> true
         | Sort_declaration _ -> false));
  assert_true "explicit aggregate named-sort declaration changed"
    (match (named_sorts, aggregate_symbol_range named) with
    | [ declared ], Logic_ir.Named range ->
        String.equal
          (Logic_ir.View.named_sort_name declared)
          "RankType_7_Record"
        && Logic_ir.View.named_sort_index declared
           = Logic_ir.View.named_sort_index range
    | [], _
    | [ _ ], (Logic_ir.Int | Logic_ir.Bool)
    | _ :: _ :: _, (Logic_ir.Int | Logic_ir.Bool | Logic_ir.Named _) ->
        false)

let test_bridge_parity config =
  let x = symbol 40 "x" Vir.Integer in
  let b = symbol 41 "b" Vir.Boolean in
  let x_term = integer_symbol x in
  let b_term = boolean_symbol b in
  let record_type = aggregate_type 7 "Record" in
  let nested_type = aggregate_type 8 "Nested" in
  let record_symbol = symbol 42 "record" (Vir.Aggregate record_type) in
  let nested_symbol = symbol 43 "nested" (Vir.Aggregate nested_type) in
  let record = aggregate_symbol record_type record_symbol in
  let nested = aggregate_symbol nested_type nested_symbol in
  test_aggregate_sort_policy record;
  let integer_field = selector record_type Vir.Integer 0 "count" [ 0 ] in
  let boolean_field = selector record_type Vir.Boolean 1 "ready" [ 1 ] in
  let nested_field =
    selector record_type (Vir.Aggregate nested_type) 2 "nested" [ 2 ]
  in
  let selected_nested : Vir.aggregate_term =
    {
      aggregate_type = nested_type;
      aggregate_desc = Vir.Aggregate_selector (nested_field, record);
    }
  in
  assert_exact_projection_parity config "unconstrained integer projection" x
    (Expected_integer Z.zero)
    (obligation ~function_index:10 ~projection_symbols:[ x ]
       (Vir.Boolean_constant false));
  assert_exact_projection_parity config "unconstrained Boolean projection" b
    (Expected_boolean false)
    (obligation ~function_index:11 ~projection_symbols:[ b ]
       (Vir.Boolean_constant false));
  assert_exact_projection_parity config "unconstrained aggregate projection"
    record_symbol
    (Expected_aggregate_identity Z.zero)
    (obligation ~function_index:12 ~projection_symbols:[ record_symbol ]
       (Vir.Boolean_constant false));
  let fixtures =
    [
      ("true", obligation (Vir.Boolean_constant true));
      ("false", obligation (Vir.Boolean_constant false));
      ( "integer-add",
        obligation
          (equal
             (Vir.Integer_add (integer (Z.of_int 2), integer (Z.of_int 3)))
             (integer (Z.of_int 5))) );
      ( "integer-subtract-negate",
        obligation
          (equal
             (Vir.Integer_subtract
                (integer (Z.of_int 2), Vir.Integer_negate (integer Z.one)))
             (integer (Z.of_int 3))) );
      ( "integer-scale",
        obligation
          (equal
             (Vir.Integer_multiply_constant
                (Z.of_int 17, integer (Z.of_int 3)))
             (integer (Z.of_int 51))) );
      ( "absolute-value",
        obligation
          (equal
             (Vir.Integer_absolute_value (integer (Z.of_int (-9))))
             (integer (Z.of_int 9))) );
      ( "Boolean-connectives",
        obligation
          (Vir.Boolean_and
             ( Vir.Boolean_not (Vir.Boolean_constant false),
               Vir.Boolean_or
                 (Vir.Boolean_constant false, Vir.Boolean_constant true) )) );
      ( "Boolean-equality",
        obligation
          (Vir.Boolean_equal
             (Vir.Boolean_constant true, Vir.Boolean_constant true)) );
      ( "all-comparisons",
        obligation
          (Vir.Boolean_and
             ( compare Vir.Less_than (integer Z.zero) (integer Z.one),
               Vir.Boolean_and
                 ( compare Vir.Less_or_equal (integer Z.one) (integer Z.one),
                   Vir.Boolean_and
                     ( compare Vir.Greater_than (integer Z.one) (integer Z.zero),
                       compare Vir.Greater_or_equal (integer Z.one)
                         (integer Z.one) ) ) )) );
      ( "integer-selector",
        obligation
          ~assumptions:
            [
              equal
                (Vir.Integer_selector (integer_field, record))
                (integer (Z.of_int 12));
            ]
          (equal
             (Vir.Integer_selector (integer_field, record))
             (integer (Z.of_int 12))) );
      ( "Boolean-selector",
        obligation
          ~assumptions:
            [
              Vir.Boolean_equal
                ( Vir.Boolean_selector (boolean_field, record),
                  Vir.Boolean_constant true );
            ]
          (Vir.Boolean_selector (boolean_field, record)) );
      ( "aggregate-selector-equality",
        obligation
          ~assumptions:[ Vir.Aggregate_equal (selected_nested, nested) ]
          (Vir.Aggregate_equal (nested, selected_nested)) );
      ( "aggregate-tag",
        obligation
          (equal
             (Vir.Aggregate_tag (record_type, record))
             (Vir.Aggregate_tag (record_type, record))) );
      ( "projected-counterexample",
        obligation ~function_index:9
          ~assumptions:
            [
              equal x_term (integer (Z.of_int 42));
              Vir.Boolean_equal (b_term, Vir.Boolean_constant true);
            ]
          ~projection_symbols:[ b; x; x ] (Vir.Boolean_constant false) );
    ]
  in
  List.iter
    (fun (label, fixture) ->
      assert_outcome_parity label (solve config fixture)
        (solve_direct config fixture))
    fixtures;
  let trivial = obligation (Vir.Boolean_constant true) in
  let direct_config : Z3_bridge.config =
    { timeout_ms = Solver_backend.timeout_ms config; model = true }
  in
  (match
     ( Solver_backend.For_testing.solve_after_translation Unknown config trivial,
       Z3_bridge.solve_vir ~controlled:Z3_bridge.Force_unknown direct_config
         trivial )
   with
  | Ok ordinary, Ok direct ->
      assert_outcome_parity "controlled unknown" ordinary direct
  | _ -> fail "controlled unknown parity failed");
  (match
     ( Solver_backend.For_testing.solve_after_translation Backend_failure config
         trivial,
       Z3_bridge.solve_vir ~controlled:Z3_bridge.Force_backend_failure
         direct_config trivial )
   with
  | Error (Solver_backend.Backend_failure _),
    Error (Z3_bridge.Backend_failure _) ->
      ()
  | _ -> fail "controlled backend-failure parity failed")

let expect_bridge_error label predicate = function
  | Error error when predicate error -> ()
  | Error _ -> fail "%s: wrong direct-Z3 bridge error" label
  | Ok _ -> fail "%s: failure was accepted" label

let test_bridge_capability_cleanup_and_isolation config =
  let direct_config : Z3_bridge.config =
    { timeout_ms = Solver_backend.timeout_ms config; model = true }
  in
  let trivial = obligation (Vir.Boolean_constant true) in
  Z3_bridge.reset_counters ();
  (match
     Z3_bridge.solve_vir
       ~requires:[ Logic_ir.Nonlinear_integer_arithmetic ]
       direct_config trivial
   with
  | Ok Z3_bridge.Verified -> ()
  | Ok _ -> fail "nonlinear capability returned an unexpected outcome"
  | Error _ -> fail "nonlinear capability was rejected");
  let nonlinear = Z3_bridge.counters () in
  assert_true "nonlinear capability did not reach a balanced solver"
    (nonlinear.capability_resolutions = 1
    && nonlinear.translations = 1
    && nonlinear.contexts_created = 1
    && nonlinear.solvers_created = 1
    && nonlinear.solver_resets = 1
    && nonlinear.contexts_cleaned = 1);
  let wrong_sort = symbol 99 "wrong_sort" Vir.Integer in
  Z3_bridge.solve_vir direct_config
    (obligation (Vir.Boolean_symbol wrong_sort))
  |> expect_bridge_error "malformed VIR" (function
       | Z3_bridge.Malformed_vir _ -> true
       | _ -> false);
  let malformed = Z3_bridge.counters () in
  assert_true "malformed VIR reached context creation"
    (malformed.translations = 2
    && malformed.contexts_created = 1
    && malformed.solvers_created = 1);
  Z3_bridge.solve_vir ~controlled:Z3_bridge.Force_unknown direct_config trivial
  |> (function
       | Ok (Z3_bridge.Inconclusive _) -> ()
       | Ok _ -> fail "controlled direct unknown was misclassified"
       | Error _ -> fail "controlled direct unknown failed");
  Z3_bridge.solve_vir ~controlled:Z3_bridge.Force_backend_failure direct_config
    trivial
  |> expect_bridge_error "controlled direct failure" (function
       | Z3_bridge.Backend_failure _ -> true
       | _ -> false);
  ignore (solve_direct config trivial);
  ignore (solve_direct config trivial);
  let final = Z3_bridge.counters () in
  assert_true "direct contexts were mixed, leaked, or not reset"
    (final.contexts_created = 5
    && final.solvers_created = 5
    && final.solver_resets = 5
    && final.contexts_cleaned = 5
    && final.contexts_live = 0
    && final.maximum_contexts_live = 1
    && final.selected_logics
       = [ "AUFNIA"; "AUFLIA"; "AUFLIA"; "AUFLIA"; "AUFLIA" ])

let quantified_query () =
  let builder = Logic_ir.create () in
  let fuel =
    Logic_ir.declare_sort builder ~name:"Fuel" ~span |> function
    | Ok sort -> sort
    | Error error -> fail "%s" (Logic_ir.error_to_string error)
  in
  let declare name domain range =
    Logic_ir.declare_function builder ~name ~domain ~range ~span |> function
    | Ok function_ -> function_
    | Error error -> fail "%s" (Logic_ir.error_to_string error)
  in
  let succ = declare "fuel_succ" [ fuel ] fuel in
  let helper = declare "observe$fuel" [ Logic_ir.Int; fuel ] Logic_ir.Int in
  let x =
    Logic_ir.bind builder ~name:"x" ~sort:Logic_ir.Int ~span |> function
    | Ok binder -> binder
    | Error error -> fail "%s" (Logic_ir.error_to_string error)
  in
  let f =
    Logic_ir.bind builder ~name:"fuel" ~sort:fuel ~span |> function
    | Ok binder -> binder
    | Error error -> fail "%s" (Logic_ir.error_to_string error)
  in
  let x_term = Logic_ir.bound x in
  let f_term = Logic_ir.bound f in
  let succ_term =
    Logic_ir.apply ~span succ [ f_term ] |> function
    | Ok term -> term
    | Error error -> fail "%s" (Logic_ir.error_to_string error)
  in
  let helper_term =
    Logic_ir.apply ~span helper [ x_term; succ_term ] |> function
    | Ok term -> term
    | Error error -> fail "%s" (Logic_ir.error_to_string error)
  in
  let body =
    Logic_ir.equal ~span helper_term helper_term |> function
    | Ok term -> term
    | Error error -> fail "%s" (Logic_ir.error_to_string error)
  in
  let axiom =
    Logic_ir.forall builder ~binders:[ x; f ] ~body
      ~patterns:[ [ helper_term; succ_term ] ]
      ~qid:"verocaml.observe.probe"
      ~skid:"verocaml.observe.probe.skolem" ~span
    |> function
    | Ok axiom -> axiom
    | Error error -> fail "%s" (Logic_ir.error_to_string error)
  in
  Logic_ir.query builder ~axioms:[ axiom ]
    ~assertions:[ Logic_ir.bool ~span true ]
    ~requires:[ Logic_ir.Models ] ~span
  |> function
  | Ok query -> query
  | Error error -> fail "%s" (Logic_ir.error_to_string error)

let run_quantifier_render () =
  let config = configured 60000 in
  let query = quantified_query () in
  let declarations, rendered =
    let direct_config : Z3_bridge.config =
      { timeout_ms = Solver_backend.timeout_ms config; model = true }
    in
    match Z3_bridge.render_query direct_config query with
    | Ok rendered -> rendered
    | Error _ -> fail "quantifier rendering failed"
  in
  print_endline declarations;
  print_endline "---";
  print_string rendered

let test_quantifier_profile config =
  let query = quantified_query () in
  let expected_diagnostic =
    "logic=AUFLIA timeout-ms=60000 model=true \
     requirements=named-sorts,uninterpreted-functions,quantifiers,\
     explicit-patterns,quantifier-ids,models"
  in
  assert_true "direct bridge diagnostic snapshot changed"
    (String.equal
       (Z3_bridge.diagnostic_snapshot
          {
            timeout_ms = Solver_backend.timeout_ms config;
            model = true;
          }
          query)
       expected_diagnostic);
  (match
     Z3_bridge.solve_query
       {
         timeout_ms = Solver_backend.timeout_ms config;
         model = true;
       }
       query
   with
  | Ok (Z3_bridge.Counterexample _) -> ()
  | Ok _ -> fail "satisfiable quantified probe was misclassified"
  | Error _ -> fail "satisfiable quantified probe failed");
  let counters = Z3_bridge.counters () in
  assert_true "actual direct solver factory did not select AUFLIA"
    (match List.rev counters.selected_logics with
    | "AUFLIA" :: _ -> true
    | [] | _ :: _ -> false);
  let major, minor, build, _ = Z3_bridge.version () in
  assert_true "direct bridge is not linked to Z3 4.15.2"
    (major = 4 && minor = 15 && build = 2)

let run_unit () =
  let config = configured 60000 in
  assert_true "AUFLIA/parameter snapshot changed"
    (String.equal
       (Solver_backend.For_testing.parameter_snapshot config)
       (Printf.sprintf "logic=AUFLIA timeout-ms=60000 rlimit=%d model=true"
          (Solver_backend.rlimit config)));
  test_raw_large_integers config;
  test_abs_minimum_overflow config;
  test_outcomes_and_projection config;
  test_preceding_safety_and_order config;
  test_unknown_errors_and_cleanup config;
  test_bridge_parity config;
  test_bridge_capability_cleanup_and_isolation config;
  test_quantifier_profile config;
  print_endline
    "solver backend unit checks: smt.ml/direct-Z3 parity, ordinary-aggregate-int, \
     explicit-named-sort, typed AUFLIA/AUFNIA, projection, capability preflight, \
     cleanup, isolation, Z3 4.15.2"

let run_timeout_smoke () =
  let config = configured 1 in
  (match
     Solver_backend.solve_obligation config
       (obligation (Vir.Boolean_constant true))
   with
  | Ok
      (Solver_backend.Verified | Counterexample _ | Inconclusive _) ->
      ()
  | Error error -> fail "%s" (Solver_backend.error_to_string error));
  print_endline "real 1ms timeout smoke: backend returned a classified outcome"

let run_invalid_config () =
  match Solver_backend.config ~timeout_ms:0 with
  | Ok _ -> fail "zero timeout was accepted"
  | Error error -> print_endline (Solver_backend.error_to_string error)

let () =
  match Array.to_list Sys.argv with
  | [ _; "unit" ] -> run_unit ()
  | [ _; "timeout-smoke" ] -> run_timeout_smoke ()
  | [ _; "invalid-config" ] -> run_invalid_config ()
  | [ _; "quantifier-render" ] -> run_quantifier_render ()
  | _ ->
      fail
        "usage: solver_backend_tool \
         (unit|timeout-smoke|invalid-config|quantifier-render)"
