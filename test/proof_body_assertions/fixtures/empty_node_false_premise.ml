type 'a node = Empty | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let lemma_false_empty_premise (node : int node [@finite]) =
  [%verocaml.requires spec_node_len node = 1];
  [%verocaml.reveal_with_fuel (spec_node_len, 4)];
  match node with
  | Empty -> [%verocaml.assert spec_node_len node = 2]
  | Node _ -> ()
[@@verocaml.proof]
