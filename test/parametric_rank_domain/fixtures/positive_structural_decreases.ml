type seed = Zero | Succ of seed
type 'a seq = Nil | Cons of 'a * 'a seq

let rec length (xs : 'a seq) : int =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (_, tail) -> 1 + length tail
[@@verocaml.spec] [@@verocaml.revealed]

let int_length (xs : int seq) = length xs [@@verocaml.spec]
let bool_length (xs : bool seq) = length xs [@@verocaml.spec]
let seed_length (xs : seed seq) = length xs [@@verocaml.spec]
