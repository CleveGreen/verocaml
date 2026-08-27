type node = Empty | Node of int * node

let rec spec_node_len (node : node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec left = Node (0, right)
and right = Node (1, left)

let rejected () =
  [%verocaml.assert spec_node_len left = 0];
  ()
