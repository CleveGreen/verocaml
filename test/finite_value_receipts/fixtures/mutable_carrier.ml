type node = Empty | Node of int * node
type stack = { mutable top : node }

let rec spec_node_len node =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let verified (stack : stack) =
  [%verocaml.assert spec_node_len stack.top >= 0];
  ()
