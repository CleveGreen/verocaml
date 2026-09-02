open Vstd

let rec advance (start : Int.t) (steps : Int.t) : Int.t =
  [%verocaml.decreases steps];
  if steps <= 0 then start else advance (start + 1) (steps - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let unfold_one_step (start : int) : unit =
  [%verocaml.ensures fun _result -> advance start 1 = start + 1];
  [%verocaml.reveal_with_fuel (advance, 2)]
[@@verocaml.proof]
