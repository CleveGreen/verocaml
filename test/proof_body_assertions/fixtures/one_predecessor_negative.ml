type node = Empty | Node of int * node

let rec spec_node_len (node : node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let one_predecessor (take_left : bool) =
  if take_left then (
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert spec_node_len Empty = 0])
  else ();
  [%verocaml.assert spec_node_len Empty >= 0]
[@@verocaml.proof]
