(** Immutable mathematical sequences for specifications and proofs. *)

type +'a t

[%%verocaml.symbolic val empty : 'a t]
[%%verocaml.symbolic val init : Int.t -> (Int.t -> 'a) -> 'a t]
[%%verocaml.symbolic val length : 'a t -> Int.t]
[%%verocaml.symbolic val get : 'a t -> Int.t -> 'a]
[%%verocaml.symbolic val push : 'a t -> 'a -> 'a t]
[%%verocaml.symbolic val update : 'a t -> Int.t -> 'a -> 'a t]
[%%verocaml.symbolic val subrange : 'a t -> Int.t -> Int.t -> 'a t]
[%%verocaml.symbolic val append : 'a t -> 'a t -> 'a t]

val valid_length : Int.t -> bool [@@verocaml.spec]
val valid_index : 'a t -> Int.t -> bool [@@verocaml.spec]
val valid_subrange : 'a t -> Int.t -> Int.t -> bool [@@verocaml.spec]
val is_empty : 'a t -> bool [@@verocaml.spec]
val first : 'a t -> 'a [@@verocaml.spec]
val last : 'a t -> 'a [@@verocaml.spec]
val singleton : 'a -> 'a t [@@verocaml.spec]
val take : 'a t -> Int.t -> 'a t [@@verocaml.spec]
val drop : 'a t -> Int.t -> 'a t [@@verocaml.spec]
val skip : 'a t -> Int.t -> 'a t [@@verocaml.spec]
val ext_equal : 'a t -> 'a t -> bool [@@verocaml.spec]
val contains : 'a t -> 'a -> bool [@@verocaml.spec]

val axiom_length_domain : 'a t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_init_length : Int.t -> (Int.t -> 'a) -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_init_get : Int.t -> (Int.t -> 'a) -> Int.t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_push_length : 'a t -> 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_push_get_last : 'a t -> 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_push_get_old : 'a t -> 'a -> Int.t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_update_length : 'a t -> Int.t -> 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_update_get_same : 'a t -> Int.t -> 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_update_get_other : 'a t -> Int.t -> 'a -> Int.t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_subrange_length : 'a t -> Int.t -> Int.t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_subrange_get : 'a t -> Int.t -> Int.t -> Int.t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_append_length : 'a t -> 'a t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_append_get_left : 'a t -> 'a t -> Int.t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_append_get_right : 'a t -> 'a t -> Int.t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val axiom_extensionality : 'a t -> 'a t -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

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
