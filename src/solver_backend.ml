module Raw = Smtml.Expr_raw

type config = Solver_policy_private.t

type model_value =
  | Integer of Z.t
  | Boolean of bool
  | Aggregate_identity of Z.t
  | Bit_vector of Bv_value.t

type model_binding = {
  symbol : Vir.symbol;
  value : model_value option;
}

type inconclusive_reason =
  | Resource_exhausted
  | Timed_out
  | Backend_unknown of string

type inconclusive = {
  configured_timeout_ms : int;
  configured_rlimit : int;
  reason : inconclusive_reason;
}

type outcome =
  | Verified
  | Counterexample of model_binding list
  | Inconclusive of inconclusive

type error =
  | Invalid_configuration of string
  | Malformed_vir of string
  | Backend_failure of string

type obligation_result = {
  obligation : Vir.obligation;
  outcome : outcome;
}

exception Translation_error of string

let policy_result_to_config policy =
  Result.map_error
    (fun error ->
      Invalid_configuration (Solver_policy_private.error_to_string error))
    policy

let config ~timeout_ms =
  Solver_policy_private.create_default ~timeout_ms
  |> policy_result_to_config

let config_with_rlimit ~timeout_ms ~rlimit =
  Solver_policy_private.create ~timeout_ms ~rlimit
  |> policy_result_to_config

let timeout_ms = Solver_policy_private.timeout_ms
let rlimit = Solver_policy_private.rlimit

let error_to_string = function
  | Invalid_configuration message ->
      Printf.sprintf "invalid solver configuration: %s" message
  | Malformed_vir message -> Printf.sprintf "malformed VIR: %s" message
  | Backend_failure message -> Printf.sprintf "solver backend failure: %s" message

let fail_translation format =
  Printf.ksprintf (fun message -> raise (Translation_error message)) format

let raw_zero = Raw.value (Smtml.Value.Int 0)
let raw_one = Raw.value (Smtml.Value.Int 1)
let raw_two = Raw.value (Smtml.Value.Int 2)

(* smt.ml 0.25 represents integer literals with a host [int].  Construct an
   arbitrary-precision numeral using raw binary double-and-add plus raw
   negation, so translating a VIR [Z.t] can never narrow through the host.
   The only extra host literal is the exact safe doubling coefficient 2.
   Doubling is multiplication by 2, rather than [accumulator + accumulator],
   because the 0.25 encoder recursively visits both operands and would
   otherwise expand a shared numeral exponentially. *)
let raw_integer value =
  if Z.equal value Z.zero then raw_zero
  else
    let magnitude = Z.abs value in
    let rec build bit accumulator =
      if bit < 0 then accumulator
      else
        let doubled =
          Raw.raw_binop Smtml.Ty.Ty_int Smtml.Ty.Binop.Mul raw_two accumulator
        in
        let accumulator =
          if Z.testbit magnitude bit then
            Raw.raw_binop Smtml.Ty.Ty_int Smtml.Ty.Binop.Add doubled raw_one
          else doubled
        in
        build (bit - 1) accumulator
    in
    let positive = build (Z.numbits magnitude - 1) raw_zero in
    if Z.sign value < 0 then
      Raw.raw_unop Smtml.Ty.Ty_int Smtml.Ty.Unop.Neg positive
    else positive

(* Scale a symbolic term without ever placing a compound ground term in
   coefficient position.  Each multiplication has the direct numeral 2 on its
   left, so the generated syntax is visibly linear before Z3 preprocessing. *)
let raw_scale coefficient value =
  if Z.equal coefficient Z.zero then raw_zero
  else
    let magnitude = Z.abs coefficient in
    let rec build bit accumulator =
      if bit < 0 then accumulator
      else
        let doubled =
          Raw.raw_binop Smtml.Ty.Ty_int Smtml.Ty.Binop.Mul raw_two accumulator
        in
        let accumulator =
          if Z.testbit magnitude bit then
            Raw.raw_binop Smtml.Ty.Ty_int Smtml.Ty.Binop.Add doubled value
          else doubled
        in
        build (bit - 1) accumulator
    in
    let positive = build (Z.numbits magnitude - 1) raw_zero in
    if Z.sign coefficient < 0 then
      Raw.raw_unop Smtml.Ty.Ty_int Smtml.Ty.Unop.Neg positive
    else positive

let symbol_name function_index (symbol : Vir.symbol) =
  Printf.sprintf "f%d_s%d" function_index symbol.symbol_id

let expected_type = function
  | Vir.Integer -> Smtml.Ty.Ty_int
  | Vir.Boolean -> Smtml.Ty.Ty_bool
  | Vir.Bit_vector _ ->
      fail_translation "SMTML backend cannot preserve native BV sorts"
  | Vir.Aggregate _ -> Smtml.Ty.Ty_int
  | Vir.Parametric _ ->
      invalid_arg "SMTML backend cannot preserve named parametric sorts"

let smtml_symbol function_index (symbol : Vir.symbol) =
  Smtml.Symbol.make (expected_type symbol.sort)
    (symbol_name function_index symbol)

let translate_symbol function_index expected_sort (symbol : Vir.symbol) =
  if symbol.sort <> expected_sort then
    fail_translation "symbol %s#%d has sort %s but is used as %s"
      symbol.source_name symbol.symbol_id
      (match symbol.sort with
      | Vir.Integer -> "integer"
      | Vir.Boolean -> "Boolean"
      | Vir.Bit_vector width -> "bit-vector " ^ Bv_width.to_string width
      | Vir.Aggregate aggregate ->
          Printf.sprintf "aggregate %s#%d" aggregate.aggregate_type_name
            aggregate.aggregate_type_index
      | Vir.Parametric binder ->
          "parameter " ^ Parametric_type.binder_to_string binder)
      (match expected_sort with
      | Vir.Integer -> "an integer"
      | Vir.Boolean -> "a Boolean"
      | Vir.Bit_vector width -> "bit-vector " ^ Bv_width.to_string width
      | Vir.Aggregate aggregate ->
          Printf.sprintf "aggregate %s#%d" aggregate.aggregate_type_name
            aggregate.aggregate_type_index
      | Vir.Parametric binder ->
          "parameter " ^ Parametric_type.binder_to_string binder);
  Raw.symbol (smtml_symbol function_index symbol)

let aggregate_sort_to_string aggregate =
  Printf.sprintf "%s#%d" aggregate.Vir.aggregate_type_name
    aggregate.aggregate_type_index

let selector_function_name (selector : Vir.selector) =
  let path =
    selector.selector_path |> List.map string_of_int |> String.concat "_"
  in
  let range =
    match selector.selector_range with
    | Vir.Integer -> "int"
    | Vir.Boolean -> "bool"
    | Vir.Bit_vector _ ->
        fail_translation "SMTML aggregate selector cannot preserve BV sorts"
    | Vir.Aggregate aggregate ->
        Printf.sprintf "agg%d_%s" aggregate.aggregate_type_index
          aggregate.aggregate_type_name
    | Vir.Parametric _ ->
        fail_translation "SMTML aggregate selector cannot preserve parametric sorts"
  in
  Printf.sprintf "verocaml_sel_t%d_%s_i%d_%s_p%s_r%s"
    selector.selector_domain.aggregate_type_index selector.selector_namespace
    selector.selector_index selector.selector_name path range

let aggregate_function result_type name argument =
  Raw.app (Smtml.Symbol.make result_type name) [ argument ]

let rec translate_aggregate function_index (term : Vir.aggregate_term) =
  match term.aggregate_desc with
  | Vir.Aggregate_symbol symbol ->
      translate_symbol function_index (Vir.Aggregate term.aggregate_type) symbol
  | Vir.Aggregate_imported_model_application
      _ ->
      let name, arguments =
        match Retained_model_application_private.resolve term with
        | Ok application -> application
        | Error message -> fail_translation "%s" message
      in
      let arguments =
        List.map
          (function
            | Vir.Recursive_integer_argument term ->
                translate_integer function_index term
            | Vir.Recursive_boolean_argument term ->
                translate_boolean function_index term
            | Vir.Recursive_bv_argument _ ->
                fail_translation "legacy backend cannot lower a native BV argument"
            | Vir.Recursive_aggregate_argument term ->
                translate_aggregate function_index term
            | Vir.Recursive_parametric_argument _ ->
                fail_translation "legacy backend cannot lower a parametric aggregate argument")
          arguments
      in
      Raw.app (Smtml.Symbol.make Smtml.Ty.Ty_int name) arguments
  | Vir.Aggregate_selector (selector, aggregate) ->
      if selector.selector_domain <> aggregate.aggregate_type then
        fail_translation "selector %s domain %s applied to aggregate %s"
          selector.selector_name
          (aggregate_sort_to_string selector.selector_domain)
          (aggregate_sort_to_string aggregate.aggregate_type);
      if selector.selector_range <> Vir.Aggregate term.aggregate_type then
        fail_translation "selector %s has non-aggregate range"
          selector.selector_name;
      aggregate_function Smtml.Ty.Ty_int
        (selector_function_name selector)
        (translate_aggregate function_index aggregate)
  | Vir.Aggregate_recursive_spec_application _ ->
      fail_translation
        "aggregate recursive specification applications require verified direct-Z3 authority"
  | Vir.Aggregate_symbolic_application _ ->
      fail_translation
        "symbolic applications require direct Logic-IR/Z3 authority"
  | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
  | Vir.Aggregate_conditional _ ->
      fail_translation
        "aggregate logical construction requires named-sort direct-Z3 authority"

and translate_selector result_type function_index selector aggregate =
  (match aggregate.Vir.aggregate_desc with
  | Vir.Aggregate_imported_model_application _ ->
      fail_translation
        "retained aggregate model applications cannot authorize selectors"
  | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
  | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
  | Vir.Aggregate_conditional _
  | Vir.Aggregate_recursive_spec_application _ ->
      ()
  | Vir.Aggregate_symbolic_application _ ->
      fail_translation
        "symbolic applications cannot authorize selectors");
  if selector.Vir.selector_domain <> aggregate.Vir.aggregate_type then
    fail_translation "selector %s domain %s applied to aggregate %s"
      selector.selector_name
      (aggregate_sort_to_string selector.selector_domain)
      (aggregate_sort_to_string aggregate.aggregate_type);
  let expected_range =
    match result_type with
    | Smtml.Ty.Ty_int -> Vir.Integer
    | Smtml.Ty.Ty_bool -> Vir.Boolean
    | _ -> assert false
  in
  if selector.Vir.selector_range <> expected_range then
    fail_translation "selector %s is used at the wrong scalar range"
      selector.selector_name;
  aggregate_function result_type (selector_function_name selector)
    (translate_aggregate function_index aggregate)

and translate_integer function_index = function
  | Vir.Integer_constant value -> raw_integer value
  | Vir.Integer_symbol symbol ->
      translate_symbol function_index Vir.Integer symbol
  | Vir.Integer_add (left, right) ->
      Raw.raw_binop Smtml.Ty.Ty_int Smtml.Ty.Binop.Add
        (translate_integer function_index left)
        (translate_integer function_index right)
  | Vir.Integer_subtract (left, right) ->
      Raw.raw_binop Smtml.Ty.Ty_int Smtml.Ty.Binop.Sub
        (translate_integer function_index left)
        (translate_integer function_index right)
  | Vir.Integer_negate value ->
      Raw.raw_unop Smtml.Ty.Ty_int Smtml.Ty.Unop.Neg
        (translate_integer function_index value)
  | Vir.Integer_multiply (left, right) ->
      Raw.raw_binop Smtml.Ty.Ty_int Smtml.Ty.Binop.Mul
        (translate_integer function_index left)
        (translate_integer function_index right)
  | Vir.Integer_multiply_constant (coefficient, value) ->
      raw_scale coefficient (translate_integer function_index value)
  | Vir.Integer_absolute_value value ->
      let value = translate_integer function_index value in
      let negative =
        Raw.raw_relop Smtml.Ty.Ty_int Smtml.Ty.Relop.Lt value raw_zero
      in
      let negated =
        Raw.raw_unop Smtml.Ty.Ty_int Smtml.Ty.Unop.Neg value
      in
      (* In smt.ml 0.25 ITE is dispatched through the Boolean implementation,
         even when its branches are integer-valued. *)
      Raw.raw_triop Smtml.Ty.Ty_bool Smtml.Ty.Triop.Ite negative negated value
  | (Vir.Integer_conditional (condition, consequent, alternative) as term)
    when Vir_integer_conditional_private.authenticate term ->
      Raw.raw_triop Smtml.Ty.Ty_bool Smtml.Ty.Triop.Ite
        (translate_boolean function_index condition)
        (translate_integer function_index consequent)
        (translate_integer function_index alternative)
  | Vir.Integer_conditional _ ->
      fail_translation
        "integer conditional lacks private authentication"
  | Vir.Aggregate_tag (aggregate_type, aggregate) ->
      (match aggregate.aggregate_desc with
      | Vir.Aggregate_imported_model_application _ ->
          fail_translation
            "retained aggregate model applications cannot authorize tags"
      | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
      | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
      | Vir.Aggregate_conditional _
      | Vir.Aggregate_recursive_spec_application _ ->
          ()
      | Vir.Aggregate_symbolic_application _ ->
          fail_translation
            "symbolic applications cannot authorize tags");
      if aggregate_type <> aggregate.aggregate_type then
        fail_translation "tag domain %s applied to aggregate %s"
          (aggregate_sort_to_string aggregate_type)
          (aggregate_sort_to_string aggregate.aggregate_type);
      aggregate_function Smtml.Ty.Ty_int
        (Printf.sprintf "verocaml_tag_t%d_%s"
           aggregate_type.aggregate_type_index aggregate_type.aggregate_type_name)
        (translate_aggregate function_index aggregate)
  | Vir.Integer_selector (selector, aggregate) ->
      translate_selector Smtml.Ty.Ty_int function_index selector aggregate
  | Vir.Integer_rank_project _ ->
      fail_translation
        "structural rank terms require verified direct-Z3 authority"
  | Vir.Integer_recursive_spec_application _ ->
      fail_translation
        "recursive specification applications require verified direct-Z3 authority"
  | Vir.Integer_symbolic_application _ ->
      fail_translation
        "symbolic applications require direct Logic-IR/Z3 authority"
  | Vir.Integer_bv_to_int_unsigned _ | Vir.Integer_bv_to_int_signed _ ->
      fail_translation
        "native BV projections require direct Logic-IR/Z3 authority"

and translate_comparison function_index comparison left right =
  let left = translate_integer function_index left in
  let right = translate_integer function_index right in
  match comparison with
  | Vir.Equal ->
      Raw.raw_relop Smtml.Ty.Ty_int Smtml.Ty.Relop.Eq left right
  | Vir.Not_equal ->
      Raw.raw_relop Smtml.Ty.Ty_int Smtml.Ty.Relop.Ne left right
  | Vir.Less_than ->
      Raw.raw_relop Smtml.Ty.Ty_int Smtml.Ty.Relop.Lt left right
  | Vir.Less_or_equal ->
      Raw.raw_relop Smtml.Ty.Ty_int Smtml.Ty.Relop.Le left right
  | Vir.Greater_than ->
      Raw.raw_relop Smtml.Ty.Ty_int Smtml.Ty.Relop.Lt right left
  | Vir.Greater_or_equal ->
      Raw.raw_relop Smtml.Ty.Ty_int Smtml.Ty.Relop.Le right left

and translate_boolean function_index = function
  | Vir.Forall_term _ | Vir.Exists_term _ ->
      fail_translation
        "user quantifier leaked into the legacy SMTML backend"
  | Vir.Boolean_constant value ->
      Raw.value
        (if value then Smtml.Value.True else Smtml.Value.False)
  | Vir.Logical_adt_schema _ -> Raw.value Smtml.Value.True
  | Vir.Boolean_symbol symbol ->
      translate_symbol function_index Vir.Boolean symbol
  | Vir.Boolean_not value ->
      Raw.raw_unop Smtml.Ty.Ty_bool Smtml.Ty.Unop.Not
        (translate_boolean function_index value)
  | Vir.Boolean_and (left, right) ->
      Raw.raw_binop Smtml.Ty.Ty_bool Smtml.Ty.Binop.And
        (translate_boolean function_index left)
        (translate_boolean function_index right)
  | Vir.Boolean_or (left, right) ->
      Raw.raw_binop Smtml.Ty.Ty_bool Smtml.Ty.Binop.Or
        (translate_boolean function_index left)
        (translate_boolean function_index right)
  | Vir.Integer_compare (comparison, left, right) ->
      translate_comparison function_index comparison left right
  | Vir.Boolean_equal (left, right) ->
      Raw.raw_relop Smtml.Ty.Ty_bool Smtml.Ty.Relop.Eq
        (translate_boolean function_index left)
        (translate_boolean function_index right)
  | Vir.Boolean_not_equal (left, right) ->
      Raw.raw_relop Smtml.Ty.Ty_bool Smtml.Ty.Relop.Ne
        (translate_boolean function_index left)
        (translate_boolean function_index right)
  | Vir.Bv_equal _ | Vir.Bv_not_equal _ | Vir.Bv_compare _ ->
      fail_translation "SMTML backend cannot lower native BV equality"
  | Vir.Boolean_selector (selector, aggregate) ->
      translate_selector Smtml.Ty.Ty_bool function_index selector aggregate
  | Vir.Aggregate_equal (left, right) ->
      if left.aggregate_type <> right.aggregate_type then
        fail_translation "aggregate equality crosses types %s and %s"
          (aggregate_sort_to_string left.aggregate_type)
          (aggregate_sort_to_string right.aggregate_type);
      Raw.raw_relop Smtml.Ty.Ty_int Smtml.Ty.Relop.Eq
        (translate_aggregate function_index left)
        (translate_aggregate function_index right)
  | Vir.Boolean_invariant_application { invariant_id; value; _ } ->
      (match value.aggregate_desc with
      | Vir.Aggregate_imported_model_application _ ->
          fail_translation
            "retained aggregate model applications cannot authorize invariants"
      | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
      | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
      | Vir.Aggregate_conditional _
      | Vir.Aggregate_recursive_spec_application _ ->
          ()
      | Vir.Aggregate_symbolic_application _ ->
          fail_translation
            "symbolic applications cannot authorize invariants");
      aggregate_function Smtml.Ty.Ty_bool
        ("verocaml_invariant_"
        ^ Digest.to_hex (Digest.string invariant_id))
        (translate_aggregate function_index value)
  | Vir.Parametric_equal _ ->
      fail_translation "SMTML backend cannot preserve parametric equality"
  | Vir.Boolean_recursive_spec_application _ ->
      fail_translation
        "recursive specification applications require verified direct-Z3 authority"
  | Vir.Boolean_specification_application _ ->
      fail_translation
        "explicit trigger applications require direct Logic-IR/Z3 authority"
  | Vir.Boolean_symbolic_application _ ->
      fail_translation
        "symbolic applications require direct Logic-IR/Z3 authority"
  | Vir.Callback_requires application ->
      translate_callback_relation function_index
        (Sst_callback_private.requires_relation application.callback)
        application.arguments
  | Vir.Callback_ensures { application; result } ->
      translate_callback_relation function_index
        (Sst_callback_private.ensures_relation application.callback)
        (application.arguments @ [ result ])

and translate_recursive_argument function_index = function
  | Vir.Recursive_integer_argument term ->
      translate_integer function_index term
  | Vir.Recursive_boolean_argument term ->
      translate_boolean function_index term
  | Vir.Recursive_bv_argument _ ->
      fail_translation "SMTML backend cannot lower a native BV argument"
  | Vir.Recursive_aggregate_argument term ->
      translate_aggregate function_index term
  | Vir.Recursive_parametric_argument _ ->
      fail_translation
        "SMTML backend cannot preserve a parametric callback relation argument"

and translate_callback_relation function_index name arguments =
  Raw.app
    (Smtml.Symbol.make Smtml.Ty.Ty_bool name)
    (List.map (translate_recursive_argument function_index) arguments)

let compare_projection_symbols (left : Vir.symbol) (right : Vir.symbol) =
  let by_id = Int.compare left.symbol_id right.symbol_id in
  if by_id <> 0 then by_id
  else
    let by_name = String.compare left.source_name right.source_name in
    if by_name <> 0 then by_name else Stdlib.compare left.sort right.sort

let normalized_projection symbols =
  let sorted = List.sort compare_projection_symbols symbols in
  let rec deduplicate previous accumulator = function
    | [] -> List.rev accumulator
    | symbol :: rest ->
        if
          match previous with
          | Some previous ->
              Int.equal previous.Vir.symbol_id symbol.Vir.symbol_id
          | None -> false
        then deduplicate previous accumulator rest
        else deduplicate (Some symbol) (symbol :: accumulator) rest
  in
  deduplicate None [] sorted

type translated_query = {
  constraints : Smtml.Expr.Set.t;
  projected : (Vir.symbol * Smtml.Symbol.t) list;
}

let translate_obligation (obligation : Vir.obligation) =
  let function_index = obligation.function_ref.function_index in
  let translate_all terms =
    List.map (translate_boolean function_index) terms
  in
  (* [required_preceding_safety] is included deliberately, even when the
     executor's current assumptions duplicate it.  This preserves the explicit
     conditional proof dependency carried by VIR. *)
  let constraints =
    translate_all obligation.assumptions
    @ translate_all obligation.required_preceding_safety
    @ translate_all obligation.path_condition
    @ [
        Raw.raw_unop Smtml.Ty.Ty_bool Smtml.Ty.Unop.Not
          (translate_boolean function_index obligation.goal);
      ]
    |> Smtml.Expr.Set.of_list
  in
  let projected =
    normalized_projection obligation.projection_symbols
    |> List.map (fun symbol ->
           (symbol, smtml_symbol function_index symbol))
  in
  { constraints; projected }

type controlled = Real | Force_unknown | Force_backend_failure

let reason_of_bridge = function
  | Z3_bridge.Resource_exhausted -> Resource_exhausted
  | Timed_out -> Timed_out
  | Backend_unknown reason -> Backend_unknown reason

let outcome_of_bridge config = function
  | Z3_bridge.Verified -> Verified
  | Counterexample bindings ->
      Counterexample
        (List.map
           (fun (binding : Z3_bridge.model_binding) ->
             let value =
               Option.map
                 (function
                   | Z3_bridge.Integer value -> Integer value
                   | Boolean value -> Boolean value
                   | Aggregate_identity value -> Aggregate_identity value
                   | Bit_vector value -> Bit_vector value)
                 binding.value
             in
             { symbol = binding.symbol; value })
           bindings)
  | Inconclusive reason ->
      Inconclusive
        {
          configured_timeout_ms = timeout_ms config;
          configured_rlimit = rlimit config;
          reason = reason_of_bridge reason;
        }

let error_of_bridge = function
  | Z3_bridge.Invalid_configuration message -> Invalid_configuration message
  | Malformed_vir message -> Malformed_vir message
  | Unsupported_features features ->
      Backend_failure
        (Printf.sprintf "unsupported direct-Z3 features: %s"
           (String.concat ", "
              (List.map Logic_ir.feature_to_string features)))
  | Malformed_logic_ir message -> Backend_failure message
  | Backend_failure message -> Backend_failure message

let solve_direct controlled config obligation =
  let controlled =
    match controlled with
    | Real -> Z3_bridge.Real
    | Force_unknown -> Force_unknown
    | Force_backend_failure -> Force_backend_failure
  in
  let direct_config =
    { Z3_bridge.timeout_ms = timeout_ms config; model = true }
  in
  let before = Z3_bridge.counters () in
  let result =
    Z3_bridge.solve_vir ~controlled ~rlimit:(rlimit config) direct_config
      obligation
  in
  let after = Z3_bridge.counters () in
  Solver_backend_counter_private.account_facade_delta ~before ~after;
  Result.map (outcome_of_bridge config)
    (Result.map_error error_of_bridge result)

let protect_solver f =
  try f () with
  | Translation_error message -> Error (Malformed_vir message)
  | exn -> Error (Backend_failure (Printexc.to_string exn))

let solve_obligation config obligation =
  protect_solver (fun () -> solve_direct Real config obligation)

let solve_in_order config obligations =
  let rec loop completed = function
    | [] -> Ok (List.rev completed)
    | obligation :: rest -> (
        match solve_obligation config obligation with
        | Error _ as error -> error
        | Ok outcome ->
            let completed = { obligation; outcome } :: completed in
            match outcome with
            | Verified -> loop completed rest
            | Counterexample _ | Inconclusive _ -> Ok (List.rev completed))
  in
  loop [] obligations

module For_testing = struct
  type controlled_failure = Unknown | Backend_failure

  let solve_after_translation controlled config obligation =
    let controlled =
      match controlled with
      | Unknown -> Force_unknown
      | Backend_failure -> Force_backend_failure
    in
    protect_solver (fun () ->
        let translated = translate_obligation obligation in
        ignore translated.constraints;
        ignore translated.projected;
        solve_direct controlled config obligation)

  let translated_integer_term ~function_index term =
    try
      Ok
        (translate_integer function_index term
        |> Smtml.Expr.to_string)
    with
    | Translation_error message -> Error (Malformed_vir message)
    | exn -> Error (Backend_failure (Printexc.to_string exn))

  let parameter_snapshot config =
    Printf.sprintf "logic=AUFLIA timeout-ms=%d rlimit=%d model=true"
      (timeout_ms config) (rlimit config)

  let reset_count = Solver_backend_counter_private.reset_count
  let solver_creation_count =
    Solver_backend_counter_private.solver_creation_count
  let reset_solver_creation_count =
    Solver_backend_counter_private.reset_solver_creation_count
end
