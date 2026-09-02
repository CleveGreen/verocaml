type node = Empty | Node of int * node

let identity (node : node) : node = node
[@@verocaml.spec]

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec cycle = Node (0, cycle)

let rejected () =
  [%verocaml.assert spec_node_len cycle = 0];
  ()
