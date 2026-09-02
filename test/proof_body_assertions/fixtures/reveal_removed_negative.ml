type node = Empty | Node of node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node next -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let reveal_removed (x : int) =
  let _marker = x in
  [%verocaml.assert spec_node_len Empty = 0]
[@@verocaml.proof]
