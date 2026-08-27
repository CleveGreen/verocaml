let rec recurse (x : int) (y : int) : int =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) (y - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let bad () : unit =
  [%verocaml.reveal_with_fuel (recurse, -1)]
[@@verocaml.proof]
