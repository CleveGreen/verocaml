type node = Empty | Node of int * node

let default_ghost_use (_value : node [@finite]) : unit = ()
[@@verocaml.proof]

let bad (value : node [@tracked]) : unit =
  default_ghost_use (value [@ghost])
[@@verocaml.proof]
