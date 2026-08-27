type 'a node = Empty | Node of 'a * 'a node
type stack = { top : int node }

let identity (stack : stack) : stack = stack
[@@verocaml.spec]

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rejected (stack : stack) =
  [%verocaml.assert spec_node_len (identity stack).top = 0];
  ()
