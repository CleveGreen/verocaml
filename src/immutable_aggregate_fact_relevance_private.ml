(** Pure relevance classification over already-issued immutable aggregate facts. *)

let rec is_exact_aggregate_construction_equality = function
  | Vir.Forall_term _ | Vir.Exists_term _ -> false
  | Vir.Aggregate_equal (left, right) ->
      let construction aggregate =
        match aggregate.Vir.aggregate_desc with
        | Vir.Aggregate_constructor _ | Vir.Aggregate_record _ -> true
        | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
        | Vir.Aggregate_imported_model_application _
        | Vir.Aggregate_conditional _
        | Vir.Aggregate_recursive_spec_application _
        | Vir.Aggregate_symbolic_application _ ->
            false
      in
      construction left || construction right
  | Vir.Boolean_constant _ | Vir.Boolean_symbol _ | Vir.Boolean_not _
  | Vir.Boolean_and _ | Vir.Boolean_or _ | Vir.Integer_compare _
  | Vir.Boolean_equal _ | Vir.Boolean_not_equal _ | Vir.Boolean_selector _
  | Vir.Bv_equal _ | Vir.Bv_not_equal _ | Vir.Bv_compare _
  | Vir.Boolean_invariant_application _
  | Vir.Logical_adt_schema _
  | Vir.Boolean_recursive_spec_application _
  | Vir.Boolean_specification_application _
  | Vir.Boolean_symbolic_application _
  | Vir.Callback_requires _ | Vir.Callback_ensures _ ->
      false
  | Vir.Parametric_equal (left, right) ->
      List.exists is_exact_aggregate_construction_equality
        (Parametric_logic_private.conditions left
        @ Parametric_logic_private.conditions right)

let rec goal_has_aggregate_equality = function
  | Vir.Forall_term quantifier | Vir.Exists_term quantifier ->
      goal_has_aggregate_equality quantifier.boolean_quantifier_body
  | Vir.Aggregate_equal _ -> true
  | Vir.Boolean_not term -> goal_has_aggregate_equality term
  | Vir.Boolean_and (left, right) | Vir.Boolean_or (left, right) ->
      goal_has_aggregate_equality left || goal_has_aggregate_equality right
  | Vir.Boolean_constant _ | Vir.Boolean_symbol _ | Vir.Integer_compare _
  | Vir.Boolean_equal _ | Vir.Boolean_not_equal _ | Vir.Boolean_selector _
  | Vir.Bv_equal _ | Vir.Bv_not_equal _ | Vir.Bv_compare _
  | Vir.Boolean_invariant_application _
  | Vir.Logical_adt_schema _
  | Vir.Boolean_recursive_spec_application _
  | Vir.Boolean_specification_application _
  | Vir.Boolean_symbolic_application _
  | Vir.Callback_requires _ | Vir.Callback_ensures _ ->
      false
  | Vir.Parametric_equal (left, right) ->
      List.exists goal_has_aggregate_equality
        (Parametric_logic_private.conditions left
        @ Parametric_logic_private.conditions right)

let rec aggregate_occurs needle term =
  needle = term
  ||
  match term.Vir.aggregate_desc with
  | Vir.Aggregate_symbol _ -> false
  | Vir.Aggregate_selector (_, source) -> aggregate_occurs needle source
  | Vir.Aggregate_imported_model_application { arguments; _ }
  | Vir.Aggregate_constructor { arguments; _ } ->
      List.exists (argument_contains needle) arguments
  | Vir.Aggregate_record { fields; _ } ->
      List.exists
        (fun (_, argument) -> argument_contains needle argument)
        fields
  | Vir.Aggregate_conditional (condition, consequent, alternative) ->
      boolean_contains needle condition
      || aggregate_occurs needle consequent
      || aggregate_occurs needle alternative
  | Vir.Aggregate_recursive_spec_application { arguments; _ } ->
      List.exists (argument_contains needle) arguments
  | Vir.Aggregate_symbolic_application application ->
      List.exists (argument_contains needle)
        (Symbolic_application_private.arguments application)

and argument_contains needle = function
  | Vir.Recursive_integer_argument term -> integer_contains needle term
  | Vir.Recursive_boolean_argument term -> boolean_contains needle term
  | Vir.Recursive_bv_argument term -> bit_vector_contains needle term
  | Vir.Recursive_aggregate_argument term -> aggregate_occurs needle term
  | Vir.Recursive_parametric_argument _ -> false

and integer_contains needle = function
  | Vir.Integer_constant _ | Vir.Integer_symbol _ -> false
  | Vir.Integer_add (left, right) | Vir.Integer_subtract (left, right) ->
      integer_contains needle left || integer_contains needle right
  | Vir.Integer_negate term
  | Vir.Integer_multiply_constant (_, term)
  | Vir.Integer_absolute_value term ->
      integer_contains needle term
  | Vir.Integer_conditional (condition, consequent, alternative) ->
      boolean_contains needle condition
      || integer_contains needle consequent
      || integer_contains needle alternative
  | Vir.Aggregate_tag (_, aggregate) | Vir.Integer_selector (_, aggregate) ->
      aggregate_occurs needle aggregate
  | Vir.Integer_recursive_spec_application { arguments; _ } ->
      List.exists (argument_contains needle) arguments
  | _ -> false

and bit_vector_contains needle term =
  match term.Vir.bit_vector_desc with
  | Vir.Bv_symbol _ | Vir.Bv_literal _ -> false
  | Vir.Bv_int_to_bv_mod { input; _ } -> integer_contains needle input
  | Vir.Bv_conditional (condition, consequent, alternative) ->
      boolean_contains needle condition
      || bit_vector_contains needle consequent
      || bit_vector_contains needle alternative
  | Vir.Bv_selector (_, aggregate) -> aggregate_occurs needle aggregate
  | Vir.Bv_not value -> bit_vector_contains needle value
  | Vir.Bv_binary (_, left, right) ->
      bit_vector_contains needle left || bit_vector_contains needle right
  | Vir.Bv_recursive_spec_application { arguments; _ } ->
      List.exists (argument_contains needle) arguments
  | Vir.Bv_symbolic_application application ->
      List.exists (argument_contains needle)
        (Symbolic_application_private.arguments application)

and boolean_contains needle = function
  | Vir.Forall_term quantifier | Vir.Exists_term quantifier ->
      let binder_is_needle =
        match needle.Vir.aggregate_desc with
        | Vir.Aggregate_symbol symbol ->
            List.exists
              (fun binder -> symbol.symbol_id = binder.Vir.symbol_id)
              quantifier.boolean_quantifier_binders
        | Vir.Aggregate_selector _ | Vir.Aggregate_imported_model_application _
        | Vir.Aggregate_constructor _ | Vir.Aggregate_record _
        | Vir.Aggregate_conditional _
        | Vir.Aggregate_recursive_spec_application _
        | Vir.Aggregate_symbolic_application _ ->
            false
      in
      (not binder_is_needle)
      && boolean_contains needle quantifier.boolean_quantifier_body
  | Vir.Boolean_constant _ | Vir.Boolean_symbol _ | Vir.Logical_adt_schema _ ->
      false
  | Vir.Boolean_not term -> boolean_contains needle term
  | Vir.Boolean_and (left, right)
  | Vir.Boolean_or (left, right)
  | Vir.Boolean_equal (left, right)
  | Vir.Boolean_not_equal (left, right) ->
      boolean_contains needle left || boolean_contains needle right
  | Vir.Integer_compare (_, left, right) ->
      integer_contains needle left || integer_contains needle right
  | Vir.Bv_equal (left, right) | Vir.Bv_not_equal (left, right)
  | Vir.Bv_compare (_, left, right) ->
      bit_vector_contains needle left || bit_vector_contains needle right
  | Vir.Boolean_selector (_, aggregate)
  | Vir.Boolean_invariant_application { value = aggregate; _ } ->
      aggregate_occurs needle aggregate
  | Vir.Aggregate_equal (left, right) ->
      aggregate_occurs needle left || aggregate_occurs needle right
  | Vir.Parametric_equal (left, right) ->
      List.exists (boolean_contains needle)
        (Parametric_logic_private.conditions left
        @ Parametric_logic_private.conditions right)
  | Vir.Boolean_recursive_spec_application { arguments; _ }
  | Vir.Boolean_specification_application { arguments; _ } ->
      List.exists (argument_contains needle) arguments
  | Vir.Boolean_symbolic_application application ->
      List.exists (argument_contains needle)
        (Symbolic_application_private.arguments application)
  | Vir.Callback_requires _ | Vir.Callback_ensures _ -> false

let rec facts_relevant_to_terms facts terms =
  let construction aggregate =
    match aggregate.Vir.aggregate_desc with
    | Vir.Aggregate_constructor _ | Vir.Aggregate_record _ -> true
    | Vir.Aggregate_symbol _ | Vir.Aggregate_selector _
    | Vir.Aggregate_imported_model_application _ | Vir.Aggregate_conditional _
    | Vir.Aggregate_recursive_spec_application _
    | Vir.Aggregate_symbolic_application _ ->
        false
  in
  List.exists
    (function
      | Vir.Forall_term _ | Vir.Exists_term _ -> false
      | Vir.Aggregate_equal (left, right) -> (
          let target =
            if construction right then Some left
            else if construction left then Some right
            else None
          in
          match target with
          | Some target -> List.exists (boolean_contains target) terms
          | None -> false)
      | Vir.Boolean_constant _ | Vir.Boolean_symbol _ | Vir.Boolean_not _
      | Vir.Boolean_and _ | Vir.Boolean_or _ | Vir.Integer_compare _
      | Vir.Boolean_equal _ | Vir.Boolean_not_equal _ | Vir.Boolean_selector _
      | Vir.Bv_equal _ | Vir.Bv_not_equal _ | Vir.Bv_compare _
      | Vir.Boolean_invariant_application _
      | Vir.Logical_adt_schema _
      | Vir.Boolean_recursive_spec_application _
      | Vir.Boolean_specification_application _
      | Vir.Boolean_symbolic_application _
      | Vir.Callback_requires _ | Vir.Callback_ensures _ ->
          false
      | Vir.Parametric_equal (left, right) ->
          facts_relevant_to_terms
            (Parametric_logic_private.conditions left
            @ Parametric_logic_private.conditions right)
            terms)
    facts
