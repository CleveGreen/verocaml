(** Portable, VC-local views of authenticated parametric ADT applications. *)

type t

type error = {
  application : string;
  message : string;
}

val instantiate :
  descriptors:Parametric_adt.t list ->
  applications:(int * Parametric_type.t list) list ->
  (t list, error) result

val descriptor : t -> Parametric_adt.t
val arguments : t -> Parametric_type.t list
val application_id : t -> string
val scc_id : t -> string
val compare : t -> t -> int
val error_to_string : error -> string
