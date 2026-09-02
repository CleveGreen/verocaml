(** Unbounded mathematical integers for specifications and proofs. *)

type t = int
[@@verocaml.logical_sort]

val of_string : string -> t
[@@verocaml.integer_literal]
