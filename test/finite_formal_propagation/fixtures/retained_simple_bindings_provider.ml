type node = Empty | Node of int * node

type 'a seq = Nil | Cons of 'a * 'a seq

let rec seq_length (xs : 'a seq) : Vstd.Int.t =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (_, tail) -> 1 + seq_length tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec seq_reflexive (xs : 'a seq [@finite]) : unit =
  [%verocaml.assert xs = xs];
  [%verocaml.decreases xs];
  match xs with Nil -> () | Cons (_, tail) -> seq_reflexive tail
[@@verocaml.proof]

let int_seq_length (seed : int) : Vstd.Int.t = seq_length (Cons (seed, Nil))
[@@verocaml.spec]

let bool_seq_length (seed : bool) : Vstd.Int.t = seq_length (Cons (seed, Nil))
[@@verocaml.spec]

let int_seq_reflexive (seed : int) : unit =
  seq_reflexive (Cons (seed, Nil))
[@@verocaml.proof]

let bool_seq_reflexive (seed : bool) : unit =
  seq_reflexive (Cons (seed, Nil))
[@@verocaml.proof]

let rec node_length (value : node) : Vstd.Int.t =
  [%verocaml.decreases value];
  match value with Empty -> 0 | Node (_, tail) -> 1 + node_length tail
[@@verocaml.spec] [@@verocaml.revealed]

let acceptable (_value : node) : bool = true
[@@verocaml.spec]

let checked (left : node [@finite]) (right : node [@finite]) =
  [%verocaml.requires
    (match left with Empty -> true | Node (_, _) -> true)
    && (match right with Empty -> true | Node (_, _) -> true)];
  [%verocaml.ensures fun result ->
    result = 0
    && (match left with Empty -> true | Node (_, _) -> true)
    && (match right with Empty -> true | Node (_, _) -> true)];
  0
