let observed (_value : int) : bool = true [@@verocaml.spec]

let lemma_cycle (value : int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger]) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

[@@@verocaml.broadcast_group (cycle_left, [cycle_right; lemma_cycle])]
[@@@verocaml.broadcast_group (cycle_right, [cycle_left])]

let untouched value = value
