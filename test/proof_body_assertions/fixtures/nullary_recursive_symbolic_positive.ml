type 'a node = Empty | Node of 'a * 'a node

let rec spec_index_alt_imp (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp idx rest
[@@verocaml.spec] [@@verocaml.revealed]

let lemma_index_empty (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Empty];
      ()
  | Node _ -> ()
[@@verocaml.proof]
