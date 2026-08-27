type 'a seq = Nil | Cons of 'a * 'a seq
type payload = (int -> int) * int ref * int array
let rec length (xs : payload seq) =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (_, tail) -> 1 + length tail
[@@verocaml.spec] [@@verocaml.revealed]
