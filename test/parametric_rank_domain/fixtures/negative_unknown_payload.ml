type 'a seq = Nil | Cons of 'a * 'a seq
let rec depth (xs : < value : int > seq) =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (_, tail) -> 1 + depth tail
[@@verocaml.spec] [@@verocaml.revealed]
