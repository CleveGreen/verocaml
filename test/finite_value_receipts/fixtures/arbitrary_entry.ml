type node = Empty | Node of int * node

let rec spec_node_len (node : node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rejected (node : node) =
  [%verocaml.assert spec_node_len node >= 0];
  ()
