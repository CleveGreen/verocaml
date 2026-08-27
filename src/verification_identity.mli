(** Private compiler-authenticated callable identities for one validated CMT. *)
type t

type callable

val create :
  imports:Imported_callable.registration option ->
  implementation:Cmt_input.implementation ->
  validated:Sst_validation.validated_program ->
  (t, string) result

val find : t -> Sst.function_definition -> (callable, string) result
val resolved_path : callable -> string
val binding_uid : callable -> string
val leaf_name : callable -> string
val canonical_path_component : callable -> string
