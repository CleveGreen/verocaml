type node = Empty | Node of int * node

let default_ghost_use (_value : node [@finite]) : unit = ()
[@@verocaml.proof]

let bad (value : node) =
  [%verocaml.proof default_ghost_use value];
  ()
