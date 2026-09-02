type 'a node = Empty | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let spec_is_node (node : int node) : bool =
  match node with Empty -> false | Node _ -> true
[@@verocaml.spec]

let lemma_symbolic_node_abstains (node : int node [@finite]) =
  [%verocaml.requires spec_is_node node];
  [%verocaml.reveal_with_fuel (spec_node_len, 4)];
  match node with
  | Empty -> ()
  | Node _ -> [%verocaml.assert spec_node_len node = 0]
[@@verocaml.proof]
