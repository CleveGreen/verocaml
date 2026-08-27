type 'a seq = Nil | Cons of 'a * 'a seq
type 'a public_seq = 'a seq

val alias_reflexive : ('a public_seq [@finite]) -> unit
[@@verocaml.proof]

val int_alias_reflexive : int -> unit
[@@verocaml.proof]

val bool_alias_reflexive : bool -> unit
[@@verocaml.proof]
