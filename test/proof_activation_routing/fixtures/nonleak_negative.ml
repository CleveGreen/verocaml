type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let sibling_nonleak (take_reveal : bool) =
  if take_reveal then
    [%verocaml.reveal_with_fuel (spec_node_len, 2)];
  affirm (spec_node_len Empty = 0)
[@@verocaml.proof]
