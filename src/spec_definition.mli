type logical_type

val classify_logical_type :
  Sst.type_definition list -> Sst.typ -> logical_type option

val classify_frozen_recursive_logical_type :
  Sst.type_definition list -> Sst.typ -> logical_type option

val source_type : logical_type -> Sst.typ
val nominal_type : logical_type -> Sst.type_id option

val authenticated_model_domain :
  Sst.type_definition list ->
  Sst.function_definition ->
  Sst.type_id option

val authenticated_invariant_domain :
  Sst.type_definition list ->
  Sst.function_definition ->
  Sst.type_id option
