module M : sig type t val value : t end = struct type t = int let value = 0 end
type 'a seq = Nil | Cons of 'a * 'a seq
let rec length (xs : M.t seq) =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (_, tail) -> 1 + length tail
[@@verocaml.spec] [@@verocaml.revealed]
