type 'a bad = Stop | Step of ('a -> int) * 'a bad
let rec bad (xs : int bad) : int =
  [%verocaml.decreases xs];
  match xs with Stop -> 0 | Step (_, tail) -> bad tail
[@@verocaml.spec] [@@verocaml.opaque]
