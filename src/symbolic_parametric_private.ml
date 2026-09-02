type value = Logical_spec_evaluation_private.value =
  | Unit_value
  | Integer_value of Vir.integer_term
  | Boolean_value of Vir.boolean_term
  | Tuple_value of value list
  | Aggregate_value of Vir.aggregate_term
  | Parametric_value of Vir.parametric_term
  | Function_value of Logical_spec_evaluation_private.function_value

let aggregate_type descriptors typ =
  match Logical_spec_evaluation_private.vir_aggregate_type_of_sst descriptors typ with
  | Some aggregate_type -> Ok aggregate_type
  | None -> Error "ADT construction has no authenticated descriptor"

let is_application = function
  | Sst.Application _ -> true
  | Sst.Unit | Sst.Bool | Sst.Int | Sst.Mathematical_int | Sst.Tuple _
  | Sst.Aggregate _
  | Sst.Parameter _ -> false

let selected_field descriptors aggregate field typ =
  Logical_spec_evaluation_private.selected_parametric_value_without_state
    ~aggregate_type:
      (Logical_spec_evaluation_private.vir_aggregate_type_of_sst descriptors)
    aggregate
    (fun path sort ->
      { (Logical_spec_evaluation_private.field_selector field path sort) with
        Vir.selector_domain = aggregate.Vir.aggregate_type })
    [] typ

let selected_argument descriptors aggregate constructor index typ =
  Logical_spec_evaluation_private.selected_parametric_value_without_state
    ~aggregate_type:
      (Logical_spec_evaluation_private.vir_aggregate_type_of_sst descriptors)
    aggregate
    (fun path sort ->
      { (Logical_spec_evaluation_private.argument_selector constructor index
           path sort) with
        Vir.selector_domain = aggregate.Vir.aggregate_type })
    [] typ

let record_construction_equality ~parametric type_definitions ~record_type
    ~aggregate ~fields =
  if parametric then Ok None
  else
    Immutable_aggregate_reconstruction_private.record_construction_equality
      type_definitions ~record_type ~aggregate ~fields

let constructor_construction_equality ~parametric type_definitions ~constructor
    ~aggregate ~arguments =
  if parametric then Ok None
  else
    Immutable_aggregate_reconstruction_private.constructor_construction_equality
      type_definitions ~constructor ~aggregate ~arguments

let equality descriptors typ left right =
  match (left, right) with
  | Aggregate_value left_aggregate, Aggregate_value right_aggregate -> (
      match
        Logical_spec_evaluation_private.scalar_variant_equality descriptors typ
          left_aggregate right_aggregate
      with
      | Some equality -> Some equality
      | None -> Logical_spec_evaluation_private.equality left right)
  | _ -> Logical_spec_evaluation_private.equality left right

let type_callbacks descriptors instantiate =
  ( (fun typ ->
      Logical_spec_evaluation_private.vir_aggregate_type_of_sst descriptors
        (instantiate typ)),
    (fun typ -> Parametric_adt.option_instance descriptors (instantiate typ)) )

let recursive_measure = function
  | Vir.Integer_constant value when Z.equal value Z.zero -> Ok ()
  | _ -> Error "parametric recursion must select an authenticated direct field"
