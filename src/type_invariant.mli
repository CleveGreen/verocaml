type handle
type environment

type error = {
  span : Diagnostic.span;
  message : string;
}

val authenticate :
  Sst_validation.validated_program -> (environment, error) result

val error_to_string : error -> string
val handles : environment -> handle list
val find_for_type : environment -> Sst.type_id -> handle option
val find_for_constructor :
  environment -> Sst.function_id -> handle option
val find_for_operation :
  environment ->
  Sst.function_id ->
  (handle * Sst.abstract_operation_role) option

val invariant_id : handle -> string
val abstract_type : handle -> Sst.type_id
val certificate_id : handle -> string
val model_callable : handle -> Sst.function_id
val model_snapshot_type : handle -> Sst.typ
val predicate_callable : handle -> Sst.function_id
val predicate_digest : handle -> string

val transition_obligation_snapshot :
  Sst.function_definition -> handle -> string list
val public_operations :
  handle -> (Sst.function_id * Sst.abstract_operation_role) list
val predicate_definition : handle -> Sst.function_definition
