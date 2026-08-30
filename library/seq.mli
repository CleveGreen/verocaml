(** Immutable mathematical sequences for specifications and proofs. *)

type +'a t

val empty : 'a t [@@verocaml.spec]
val init : int -> (int -> 'a) -> 'a t [@@verocaml.spec]
val length : 'a t -> int [@@verocaml.spec]
val get : 'a t -> int -> 'a [@@verocaml.spec]
val push : 'a t -> 'a -> 'a t [@@verocaml.spec]
val update : 'a t -> int -> 'a -> 'a t [@@verocaml.spec]
val subrange : 'a t -> int -> int -> 'a t [@@verocaml.spec]
val append : 'a t -> 'a t -> 'a t [@@verocaml.spec]

val valid_length : int -> bool [@@verocaml.spec]
val valid_index : 'a t -> int -> bool [@@verocaml.spec]
val valid_subrange : 'a t -> int -> int -> bool [@@verocaml.spec]
val is_empty : 'a t -> bool [@@verocaml.spec]
val first : 'a t -> 'a [@@verocaml.spec]
val last : 'a t -> 'a [@@verocaml.spec]
val singleton : 'a -> 'a t [@@verocaml.spec]
val take : 'a t -> int -> 'a t [@@verocaml.spec]
val drop : 'a t -> int -> 'a t [@@verocaml.spec]
val skip : 'a t -> int -> 'a t [@@verocaml.spec]
val ext_equal : 'a t -> 'a t -> bool [@@verocaml.spec]
val contains : 'a t -> 'a -> bool [@@verocaml.spec]

val axiom_length_domain : 'a t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_init_length : int -> (int -> 'a) -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_init_get : int -> (int -> 'a) -> int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_push_length : 'a t -> 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_push_get_last : 'a t -> 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_push_get_old : 'a t -> 'a -> int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_update_length : 'a t -> int -> 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_update_get_same : 'a t -> int -> 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_update_get_other : 'a t -> int -> 'a -> int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_subrange_length : 'a t -> int -> int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_subrange_get : 'a t -> int -> int -> int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_append_length : 'a t -> 'a t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_append_get_left : 'a t -> 'a t -> int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_append_get_right : 'a t -> 'a t -> int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_extensionality : 'a t -> 'a t -> unit [@@verocaml.proof]

[@@@verocaml.broadcast_group
  (group_seq_axioms,
   [ axiom_length_domain;
     axiom_init_length;
     axiom_init_get;
     axiom_push_length;
     axiom_push_get_last;
     axiom_push_get_old;
     axiom_update_length;
     axiom_update_get_same;
     axiom_update_get_other;
     axiom_subrange_length;
     axiom_subrange_get;
     axiom_append_length;
     axiom_append_get_left;
     axiom_append_get_right ])]
