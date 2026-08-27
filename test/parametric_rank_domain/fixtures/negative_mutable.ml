type 'a seq = Nil | Cons of 'a * 'a seq ref
let rec length (xs : int seq) =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (_, tail) -> 1 + length !tail
[@@verocaml.spec] [@@verocaml.revealed]
