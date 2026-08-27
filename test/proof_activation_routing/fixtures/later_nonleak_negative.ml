type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let later_nonleak (_dummy : bool) =
  affirm (spec_node_len Empty = 0);
  [%verocaml.reveal_with_fuel (spec_node_len, 2)]
[@@verocaml.proof]
