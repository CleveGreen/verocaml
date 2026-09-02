open Vstd

let rec bad (f : Int.t -> Int.t) (n : Int.t) : Int.t =
  [%verocaml.decreases n];
  if n <= 0 then f n else bad f (n - 1)
[@@verocaml.spec] [@@verocaml.opaque]
