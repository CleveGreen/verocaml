type node = Empty | Node of int * node

let rec spec_node_len node =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec cycle =
  let alias = cycle in
  Node (0, alias)

let verified () =
  [%verocaml.assert spec_node_len cycle = 1];
  ()
