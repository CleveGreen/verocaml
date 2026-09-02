type node = Empty | Node of int * node

type 'a seq = Nil | Cons of 'a * 'a seq

val seq_length : 'a seq -> Vstd.Int.t
val seq_reflexive : ('a seq [@finite]) -> unit
val int_seq_length : int -> Vstd.Int.t
val bool_seq_length : bool -> Vstd.Int.t
val int_seq_reflexive : int -> unit
val bool_seq_reflexive : bool -> unit
val node_length : node -> Vstd.Int.t
val acceptable : node -> bool
val checked : (node [@finite]) -> (node [@finite]) -> int
