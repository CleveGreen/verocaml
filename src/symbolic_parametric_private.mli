type value = Logical_spec_evaluation_private.value =
  | Unit_value
  | Integer_value of Vir.integer_term
  | Boolean_value of Vir.boolean_term
  | Bit_vector_value of Vir.bit_vector_term
  | Tuple_value of value list
  | Aggregate_value of Vir.aggregate_term
  | Parametric_value of Vir.parametric_term
  | Function_value of Logical_spec_evaluation_private.function_value

val aggregate_type :
  Parametric_adt.t list -> Sst.typ -> (Vir.aggregate_type, string) result

val is_application : Sst.typ -> bool

val selected_field :
  Parametric_adt.t list ->
  Vir.aggregate_term ->
  Sst.field_id ->
  Sst.typ ->
  value

val selected_argument :
  Parametric_adt.t list ->
  Vir.aggregate_term ->
  Sst.constructor_id ->
  int ->
  Sst.typ ->
  value

val record_construction_equality :
  parametric:bool ->
  Sst.type_definition list ->
  record_type:Sst.type_id ->
  aggregate:Vir.aggregate_term ->
  fields:(Sst.field_id * Vir.recursive_spec_argument) list ->
  (Vir.boolean_term option, string) result

val constructor_construction_equality :
  parametric:bool ->
  Sst.type_definition list ->
  constructor:Sst.constructor_id ->
  aggregate:Vir.aggregate_term ->
  arguments:Vir.recursive_spec_argument list ->
  (Vir.boolean_term option, string) result

val equality :
  Parametric_adt.t list ->
  Sst.typ ->
  value ->
  value ->
  Vir.boolean_term option

val type_callbacks :
  Parametric_adt.t list ->
  (Sst.typ -> Sst.typ) ->
  ( (Sst.typ -> Vir.aggregate_type option) *
    (Sst.typ -> Parametric_adt.option_instance option) )

val recursive_measure : Vir.integer_term -> (unit, string) result
