type t

val create :
  definition:Sst.logical_constant_definition ->
  type_arguments:Parametric_type.t list ->
  result_type:Parametric_type.t ->
  span:Diagnostic.span ->
  (t, string) result

val constant_id : t -> Sst.logical_constant_id
val type_arguments : t -> Parametric_type.t list
val result_type : t -> Parametric_type.t
val span : t -> Diagnostic.span
val identity_material : t -> string
val identity_digest : t -> string
val backend_head : t -> string
val compare : t -> t -> int
val equal : t -> t -> bool
