(** Exact direct aggregate-recursive-application equality view. *)

type t

val of_goal :
  Vir.boolean_term ->
  (t option, [ `Malformed of Diagnostic.span * string ]) result

val application : t -> Vir.aggregate_term
val callee : t -> Sst.function_id
val arguments : t -> Vir.recursive_spec_argument list
val result_type : t -> Vir.aggregate_type
val application_identity : t -> Recursive_spec_application_identity.t
