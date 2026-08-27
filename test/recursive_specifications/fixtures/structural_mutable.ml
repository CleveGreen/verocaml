type 'a bad = Stop | Step of { mutable next : 'a bad }
let rec bad (xs : int bad) : int =
  [%verocaml.decreases xs];
  match xs with Stop -> 0 | Step { next } -> bad next
[@@verocaml.spec] [@@verocaml.opaque]
