(** Pure exact equalities for validated deeply immutable local aggregates. *)

val record_construction_equality :
  Sst.type_definition list ->
  record_type:Sst.type_id ->
  aggregate:Vir.aggregate_term ->
  fields:(Sst.field_id * Vir.recursive_spec_argument) list ->
  (Vir.boolean_term option, string) result

val constructor_construction_equality :
  Sst.type_definition list ->
  constructor:Sst.constructor_id ->
  aggregate:Vir.aggregate_term ->
  arguments:Vir.recursive_spec_argument list ->
  (Vir.boolean_term option, string) result

val transparent_schema_embedding :
  Parametric_adt.t list ->
  Sst.type_definition list ->
  Sst.typ ->
  bool

val pattern_reconstruction_equalities :
  Parametric_adt.t list ->
  Sst.type_definition list ->
  Sst.pattern ->
  Vir.aggregate_term ->
  (Vir.boolean_term list, string) result

val owned_tree_transitions : Sst.expression -> Sst.owned_tree_transition list
