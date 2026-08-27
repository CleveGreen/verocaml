type kind = Forall | Exists

type first_order_binder =
  | Integer_binder
  | Boolean_binder
  | Parameter_binder of Parametric_type.binder
  | Application_binder of Parametric_type.t

type t
type vector

val create :
  kind:kind ->
  owner:string ->
  binder_index:int ->
  binder_type:Parametric_type.t ->
  span:Diagnostic.span ->
  t

val kind : t -> kind
val owner : t -> string
val binder_index : t -> int
val binder_type : t -> Parametric_type.t
val span : t -> Diagnostic.span
val qid : t -> string
val skid : t -> string
val rebind : binder_index:int -> binder_type:Parametric_type.t -> t -> t
val seal_import : offset:int -> binder_index:int -> binder_type:Parametric_type.t -> t -> t
val equal : t -> t -> bool
val validate_shape :
  ?expected_owner:string ->
  kind:kind ->
  binder_index:int ->
  binder_type:Parametric_type.t ->
  t ->
  (unit, string) result
val validate_identity : t -> (unit, string) result

val vector : t list -> (vector, string) result
val singleton : t -> vector
val vector_kind : vector -> kind
val vector_owner : vector -> string
val vector_binders : vector -> t list
val vector_span : vector -> Diagnostic.span
val vector_qid : vector -> string
val vector_skid : vector -> string
val vector_equal : vector -> vector -> bool
val vector_rebind :
  (int * Parametric_type.t) list -> vector -> (vector, string) result
val vector_seal_import :
  offset:int ->
  (int * Parametric_type.t) list ->
  vector ->
  (vector, string) result
val validate_vector :
  ?expected_owner:string ->
  kind:kind ->
  binder_indices:int list ->
  binder_types:Parametric_type.t list ->
  vector ->
  (unit, string) result
val validate_vector_identity : vector -> (unit, string) result

val first_order_binder :
  Parametric_type.t -> (first_order_binder, string) result
val kind_to_string : kind -> string
val to_string : t -> string
val vector_to_string : vector -> string
