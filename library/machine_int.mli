(** Mathematical views of OCaml machine integers. The runtime representation
    remains [int]; all arithmetic in these specifications is unbounded. *)
type t = int
[@@verocaml.numeric_carrier
  { base = Int.t; profile = "portable-word";
    representation = "immediate"; compatibility = [] }]

val unsigned_range : t -> unit [@@verocaml.proof]
val unsigned : t -> Int.t [@@verocaml.spec]
[@@verocaml.numeric_role
  { carrier = t; role_schema = "numeric-role.v1"; role = "unsigned-view";
    semantics = unsigned_range; visibility = "visible"; reveal = true; inline = true }]

val signed_relation : t -> unit [@@verocaml.proof]
val signed : t -> Int.t [@@verocaml.spec]
[@@verocaml.numeric_role
  { carrier = t; role_schema = "numeric-role.v1"; role = "signed-view";
    semantics = signed_relation; visibility = "visible"; reveal = true; inline = true }]

val bounds_relation : Int.t -> unit [@@verocaml.proof]
val fits_signed : Int.t -> bool [@@verocaml.spec]
[@@verocaml.numeric_role
  { carrier = t; role_schema = "numeric-role.v1"; role = "bounds";
    semantics = bounds_relation; visibility = "visible"; reveal = true; inline = true }]

val identity : t -> int
[@@verocaml.numeric_role
  { carrier = t; role_schema = "numeric-role.v1"; role = "runtime-view";
    semantics = signed; visibility = "visible"; reveal = false; inline = false }]
