type 'a seq = Nil | Cons of 'a * 'a seq

val observe : ('a seq [@finite]) -> unit
[@@verocaml.proof]

val int_control : (int seq [@finite]) -> unit
[@@verocaml.proof]

val bool_control : (bool seq [@finite]) -> unit
[@@verocaml.proof]
