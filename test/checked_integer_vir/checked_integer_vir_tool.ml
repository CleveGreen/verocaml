let fail format = Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let span line =
  let position column = Diagnostic.{ line; column } in
  Diagnostic.
    {
      file = "unit.ml";
      start_pos = position 0;
      end_pos = position 1;
    }

let int_expression line expression_desc =
  Sst.{ expression_desc; typ = Int; span = span line }

let variable (binding : Sst.binding) =
  int_expression binding.Sst.span.start_pos.line
    (Sst.Variable
       { binding; use_uniqueness = Sst.Definitely_aliased })

let checked line operation arguments =
  int_expression line (Sst.Checked_arithmetic (operation, arguments))

let parameter_function index name body =
  let binding =
    Sst.
      {
        id = 0;
        name = "x";
        typ = Int;
        uniqueness = Definitely_aliased;
        span = span (index + 1);
      }
  in
  let pattern =
    Sst.{ pattern_desc = Bind binding; typ = Int; span = binding.span }
  in
  ( binding,
    Sst_normalize.checked_exec_raw
      ~function_id:Sst.{ function_index = index; function_name = name }
      ~recursive:false ~parameters:[ Sst.Value_parameter
        Sst.{ label = None; pattern; optional_default = None } ]
      ~contracts:Sst.empty_contracts ~body:(body binding) ~result_type:Sst.Int
      ~returns_unique_parameter:None ~span:(span (index + 1)) )

let constant_function index name body =
  Sst_normalize.checked_exec_raw
    ~function_id:Sst.{ function_index = index; function_name = name }
    ~recursive:false ~parameters:[] ~contracts:Sst.empty_contracts ~body
    ~result_type:Sst.Int ~returns_unique_parameter:None
    ~span:(span (index + 30))

let obligation_operations execution =
  List.map
    (fun obligation ->
      match obligation.Vir.kind with
      | Vir.Arithmetic_safety { operation; violated_bound; _ } ->
          (operation, violated_bound)
      | Vir.Assertion _ | Vir.Local_assertion _ | Vir.Postcondition _
      | Vir.Call_precondition _ | Vir.Callback_precondition _
      | Vir.Invariant_validity _
      | Vir.Entry_measure_nonnegative _
      | Vir.Recursive_call_measure_nonnegative _
      | Vir.Recursive_call_strict_descent _ ->
          fail "non-arithmetic obligation reached arithmetic-only fixture")
    execution.Vir.obligations

let assert_equal label expected actual =
  if expected <> actual then
    fail "%s: expected %d, found %d" label expected actual

let assert_true label condition = if not condition then fail "%s" label

let run_unit_checks () =
  assert_true "minimum bound changed"
    (Z.equal Int_bounds.minimum (Z.of_string "-4611686018427387904"));
  assert_true "maximum bound changed"
    (Z.equal Int_bounds.maximum (Z.of_string "4611686018427387903"));
  let definitions =
    [
      parameter_function 0 "add" (fun x ->
          checked 10 Sst.Add
            [ variable x; int_expression 10 (Sst.Int_constant Z.one) ]);
      parameter_function 1 "subtract" (fun x ->
          checked 11 Sst.Subtract
            [ variable x; int_expression 11 (Sst.Int_constant Z.one) ]);
      parameter_function 2 "negate" (fun x ->
          checked 12 Sst.Negate [ variable x ]);
      parameter_function 3 "multiply" (fun x ->
          checked 13 (Sst.Multiply_constant (Z.of_int 3)) [ variable x ]);
      parameter_function 4 "successor" (fun x ->
          checked 14 Sst.Successor [ variable x ]);
      parameter_function 5 "predecessor" (fun x ->
          checked 15 Sst.Predecessor [ variable x ]);
      parameter_function 6 "absolute" (fun x ->
          checked 16 Sst.Absolute_value [ variable x ]);
      parameter_function 7 "intermediate" (fun x ->
          checked 17 Sst.Add
            [
              checked 17 Sst.Add
                [ variable x; int_expression 17 (Sst.Int_constant Z.one) ];
              int_expression 17 (Sst.Int_constant Z.one);
            ]);
    ]
    |> List.map snd
  in
  let program =
    match
      Symbolic_executor.lower_program
        Sst.{ policy = Default_linear_z3; parametric_adts = []; types = []; logical_constants = []; functions = definitions }
    with
    | Ok program -> program
    | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  in
  assert_equal "function count" 8 (List.length program.Vir.functions);
  List.iter
    (fun execution ->
      let operations = obligation_operations execution in
      assert_equal "lower/upper obligation count" 2 (List.length operations);
      match operations with
      | [ (_, Vir.Lower_bound); (_, Vir.Upper_bound) ] -> ()
      | _ -> fail "bounds were not emitted lower then upper")
    (List.filter
       (fun execution ->
         not
           (String.equal execution.Vir.function_ref.function_name
              "intermediate"))
       program.functions);
  let intermediate =
    List.find
      (fun execution ->
        String.equal execution.Vir.function_ref.function_name "intermediate")
      program.functions
  in
  assert_equal "intermediate obligation count" 4
    (List.length intermediate.obligations);
  let first_lower, first_upper, second_lower, second_upper =
    match intermediate.obligations with
    | [ first_lower; first_upper; second_lower; second_upper ] ->
        (first_lower, first_upper, second_lower, second_upper)
    | _ -> fail "unexpected intermediate obligation shape"
  in
  assert_true "a lower goal justified itself"
    (not (List.mem first_lower.goal first_lower.assumptions));
  assert_true "an upper goal justified itself"
    (not (List.mem first_upper.goal first_upper.assumptions));
  assert_true "prior lower safety was not handed to the next operation"
    (List.mem first_lower.goal second_lower.assumptions);
  assert_true "prior upper safety was not handed to the next operation"
    (List.mem first_upper.goal second_lower.assumptions);
  assert_true "conditional lower handoff was not explicit"
    (List.mem first_lower.goal second_lower.required_preceding_safety);
  assert_true "conditional upper handoff was not explicit"
    (List.mem first_upper.goal second_lower.required_preceding_safety);
  assert_true "second operation lower goal justified itself"
    (not (List.mem second_lower.goal second_lower.assumptions));
  assert_true "second operation upper goal justified itself"
    (not (List.mem second_upper.goal second_upper.assumptions));
  assert_true "mathematical add folded at construction"
    (String.equal
       (Vir.integer_term_to_string
          (Vir.Integer_add
             ( Vir.Integer_constant Int_bounds.maximum,
               Vir.Integer_constant Z.one )))
       "(+ 4611686018427387903 1)");
  assert_true "mathematical subtract folded at construction"
    (String.equal
       (Vir.integer_term_to_string
          (Vir.Integer_subtract
             ( Vir.Integer_constant Int_bounds.minimum,
               Vir.Integer_constant Z.one )))
       "(- -4611686018427387904 1)");
  assert_true "minimum negation folded at construction"
    (String.equal
       (Vir.integer_term_to_string
          (Vir.Integer_negate (Vir.Integer_constant Int_bounds.minimum)))
       "(- -4611686018427387904)");
  assert_true "absolute minimum folded at construction"
    (String.equal
       (Vir.integer_term_to_string
          (Vir.Integer_absolute_value
             (Vir.Integer_constant Int_bounds.minimum)))
       "(abs -4611686018427387904)");
  let boundary_definitions =
    [
      ( "maximum-plus-one",
        Vir.Add,
        "(+ 4611686018427387903 1)",
        checked 30 Sst.Add
          [
            int_expression 30 (Sst.Int_constant Int_bounds.maximum);
            int_expression 30 (Sst.Int_constant Z.one);
          ] );
      ( "minimum-minus-one",
        Vir.Subtract,
        "(- -4611686018427387904 1)",
        checked 31 Sst.Subtract
          [
            int_expression 31 (Sst.Int_constant Int_bounds.minimum);
            int_expression 31 (Sst.Int_constant Z.one);
          ] );
      ( "negate-minimum",
        Vir.Negate,
        "(- -4611686018427387904)",
        checked 32 Sst.Negate
          [ int_expression 32 (Sst.Int_constant Int_bounds.minimum) ] );
      ( "absolute-minimum",
        Vir.Absolute_value,
        "(abs -4611686018427387904)",
        checked 33 Sst.Absolute_value
          [ int_expression 33 (Sst.Int_constant Int_bounds.minimum) ] );
      ( "multiply-maximum",
        Vir.Multiply_constant (Z.of_int 2),
        "(* 2 4611686018427387903)",
        checked 34 (Sst.Multiply_constant (Z.of_int 2))
          [ int_expression 34 (Sst.Int_constant Int_bounds.maximum) ] );
    ]
  in
  let boundary_program =
    Sst.
      {
        policy = Default_linear_z3;
        parametric_adts = [];
        types = [];
        logical_constants = [];
        functions =
          List.mapi
            (fun index (name, _, _, body) ->
              constant_function (index + 20) name body)
            boundary_definitions;
      }
  in
  let boundary_vir =
    match Symbolic_executor.lower_program boundary_program with
    | Ok program -> program
    | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  in
  List.iter2
    (fun (expected_name, expected_operation, expected_result, _) execution ->
      assert_true "boundary function provenance missing"
        (String.equal expected_name
           execution.Vir.function_ref.function_name);
      match execution.obligations with
      | [ lower; upper ] ->
          let inspect expected_bound obligation =
            match obligation.Vir.kind with
            | Vir.Arithmetic_safety
                { operation; mathematical_result; violated_bound } ->
                assert_true "boundary operation provenance changed"
                  (operation = expected_operation);
                assert_true "boundary mathematical result changed"
                  (String.equal expected_result
                     (Vir.integer_term_to_string mathematical_result));
                assert_true "boundary side changed"
                  (violated_bound = expected_bound)
            | Vir.Assertion _ | Vir.Local_assertion _
            | Vir.Postcondition _
            | Vir.Call_precondition _ | Vir.Callback_precondition _
            | Vir.Invariant_validity _
            | Vir.Entry_measure_nonnegative _
            | Vir.Recursive_call_measure_nonnegative _
            | Vir.Recursive_call_strict_descent _ ->
                fail
                  "non-arithmetic obligation reached arithmetic boundary fixture"
          in
          inspect Vir.Lower_bound lower;
          inspect Vir.Upper_bound upper;
          assert_true "boundary lower goal does not use exact minimum"
            (String.starts_with
               ~prefix:"(<= -4611686018427387904 "
               (Vir.boolean_term_to_string lower.goal));
          assert_true "boundary upper goal does not use exact maximum"
            (String.ends_with
               ~suffix:"4611686018427387903)"
               (Vir.boolean_term_to_string upper.goal))
      | _ -> fail "boundary operation did not emit exactly two obligations")
    boundary_definitions boundary_vir.functions;
  List.iter
    (fun execution ->
      List.iteri
        (fun index obligation ->
          assert_true "obligation indices are not deterministic and unique"
            (Int.equal index obligation.Vir.obligation_index))
        execution.Vir.obligations;
      List.iter
        (fun (obligation : Vir.obligation) ->
          assert_true "function provenance missing"
            (String.equal obligation.Vir.function_ref.function_name
               execution.Vir.function_ref.function_name);
          assert_true "projection symbols missing"
            (List.exists
               (fun symbol -> String.equal symbol.Vir.source_name "x")
               obligation.projection_symbols))
        execution.obligations;
      List.iter
        (fun exit ->
          match exit.Vir.result with
          | Vir.Integer_result symbol ->
              let result_term = Vir.Integer_symbol symbol in
              List.iter
                (fun range ->
                  assert_true "result range assumption missing"
                    (List.mem range exit.assumptions))
                (Vir.integer_range result_term)
          | _ -> fail "integer function did not materialize an integer result")
        execution.exits)
    program.functions;
  print_endline
    "checked integer VIR unit checks: bounds, raw terms, ordering, handoff, provenance, ranges"

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic ->
      fail "%s at %s" diagnostic.Diagnostic.code diagnostic.span.file
  | Ok sst -> (
      match Symbolic_executor.lower_program sst with
      | Ok vir -> vir
      | Error error -> fail "%s" (Symbolic_executor.error_to_string error))

let () =
  match Array.to_list Sys.argv with
  | [ _; "unit" ] -> run_unit_checks ()
  | [ _; "dump"; filename ] -> print_string (Vir.to_string (load filename))
  | _ -> fail "usage: checked_integer_vir_tool (unit|dump FILE.cmt)"
