type 'a node = Empty | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec]
[@@verocaml.revealed]

let rec make_n_nodes (value : int) (count : int) : int node =
  [%verocaml.requires count >= 0];
  [%verocaml.ensures fun result -> spec_node_len result = count + 1];
  [%verocaml.decreases count];
  if count = 0 then Empty
  else Node (value, make_n_nodes value (count - 1))
