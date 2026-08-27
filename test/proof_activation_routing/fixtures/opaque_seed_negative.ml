type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec opaque_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + opaque_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let opaque_seed_control (_dummy : bool) =
  affirm (opaque_node_len Empty = 0)
[@@verocaml.proof]
