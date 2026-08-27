type 'a seq = Nil | Cons of 'a * 'a seq

val seq_reflexive : ('a seq [@finite]) -> unit
[@@verocaml.proof]

val int_seq_reflexive : int -> unit
[@@verocaml.proof]

val bool_seq_reflexive : bool -> unit
[@@verocaml.proof]

val substituted_interface : int
