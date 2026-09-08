type t = private { width : Bv_width.t; unsigned_bits : Z.t }

val of_string : width:Bv_width.t -> string -> (t, string) result
val of_z : width:Bv_width.t -> Z.t -> (t, string) result
val reduce_int : width:Bv_width.t -> Z.t -> t
val signed_value : t -> Z.t
val render : t -> string
val canonical_decimal : t -> string
val equal : t -> t -> bool
