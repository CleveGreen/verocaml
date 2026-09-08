type owner = { owner_index : int; owner_name : string }
type binder = { owner : owner; ordinal : int }
type type_id = { type_index : int; type_name : string }
type constructor = { constructor_path : string; constructor_identity : string }

type t =
  | Unit
  | Bool
  | Int
  | Mathematical_int
  | Bit_vector of Bv_width.t
  | Tuple of (string option * t) list
  | Aggregate of type_id
  | Parameter of binder
  | Application of constructor * t list

val owner : index:int -> name:string -> owner
val binder : owner -> ordinal:int -> binder
val binders : owner -> int -> binder list
val spec_function_constructor : label:string option -> constructor
val spec_function :
  label:string option -> domain:t -> range:t -> t
val spec_function_view : t -> (string option * t * t) option
val is_spec_function : t -> bool
val is_integer : t -> bool
val contains_mathematical_int : t -> bool
val contains_bit_vector : t -> bool
val application : constructor -> t list -> (t, string) result
val validate_application : constructor -> t list -> (unit, string) result
val compare_owner : owner -> owner -> int
val compare_binder : binder -> binder -> int
val compare_constructor : constructor -> constructor -> int
val compare : t -> t -> int
val equal : t -> t -> bool

(** Whether a logical value may inhabit a compiler-declared shape after
    runtime integer leaves have been lifted to mathematical integers. *)
val compiler_erasure_compatible : compiler:t -> semantic:t -> bool
val alpha_equal : t -> t -> bool
val substitute : (binder * t) list -> t -> t
val instantiate : binder list -> t list -> t -> (t, string) result
val is_open : t -> bool
val parameters : t -> binder list
val owner_to_string : owner -> string
val binder_to_string : binder -> string
val to_string : t -> string
val structural_identity_material : t -> string
val structural_identity_digest : t -> string
val structural_vector_material : t list -> string
val structural_vector_digest : t list -> string
