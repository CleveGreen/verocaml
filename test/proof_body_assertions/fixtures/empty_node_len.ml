type 'a node = Empty | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let lemma_empty_node_len (node : int node [@finite]) =
  [%verocaml.requires spec_is_empty node];
  [%verocaml.ensures fun _ -> spec_node_len node = 0];
  [%verocaml.reveal_with_fuel (spec_node_len, 4)];
  match node with
  | Empty -> [%verocaml.assert spec_node_len node = 0]
  | Node _ -> [%verocaml.assert false]
[@@verocaml.proof]
