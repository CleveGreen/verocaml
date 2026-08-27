type 'a seq = Nil | Cons of 'a * 'a seq
type rebound = int seq
let rec depth (xs : rebound) =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (_, tail) -> 1 + depth tail
[@@verocaml.spec] [@@verocaml.revealed]
