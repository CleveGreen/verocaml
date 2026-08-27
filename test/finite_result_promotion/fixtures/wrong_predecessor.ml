type node = Empty | Node of int * node

let rec spec_node_len (node : node) : int =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let push_front (_value : int) (xs : node [@finite]) : node =
  [%verocaml.ensures fun result ->
    spec_node_len result = spec_node_len xs + 1];
  xs
