open Vstd

let rec recurse (x : Int.t) (y : Int.t) : Int.t =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) (y - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let unfold_two_layers (x : int) : unit =
  [%verocaml.ensures fun _result -> recurse x 1 = x + 1];
  [%verocaml.reveal_with_fuel (recurse, 2)]
[@@verocaml.proof]
