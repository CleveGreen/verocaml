type node = Empty | Node of int * node

let rec length (value : node) : Vstd.Int.t =
  [%verocaml.decreases value];
  match value with Empty -> 0 | Node (_, tail) -> 1 + length tail
[@@verocaml.spec] [@@verocaml.revealed]

let exec_use (value : node [@finite]) =
  [%verocaml.assert length value = length value];
  ()

let proof_use (value : node [@finite]) : unit =
  [%verocaml.assert length value = length value];
  ()
[@@verocaml.proof]

let spec_use (value : node [@finite]) : Vstd.Int.t = length value
[@@verocaml.spec]

let default_ghost_use (value : node [@finite]) : unit =
  [%verocaml.assert spec_use value = length value];
  ()
[@@verocaml.proof]

let tracked_use (value : node [@tracked] [@finite]) : unit =
  [%verocaml.assert length value = length value];
  ()
[@@verocaml.proof]

let explicit_ghost_use (value : node [@ghost] [@finite]) : unit =
  [%verocaml.assert length value = length value];
  ()
[@@verocaml.proof]

let proof_chain (value : node [@finite]) : unit =
  proof_use value;
  default_ghost_use value
[@@verocaml.proof]

let tracked_chain (value : node [@tracked] [@finite]) : unit =
  tracked_use (value [@tracked]);
  default_ghost_use (value [@ghost])
[@@verocaml.proof]

let ghost_chain (value : node [@ghost] [@finite]) : unit =
  explicit_ghost_use value;
  default_ghost_use value
[@@verocaml.proof]

let closed_ghost_caller (seed : int) : unit =
  explicit_ghost_use (Node (seed, Empty))
[@@verocaml.proof]

let exec_caller () = exec_use (Node (1, Empty))

let exec_to_default_ghost (value : node [@finite]) =
  [%verocaml.proof default_ghost_use value];
  ()
