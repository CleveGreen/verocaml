type 'a node = Empty | Other | Node of 'a * 'a node

let rec spec_index_alt_imp (idx : Vstd.Int.t) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Other -> Other
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp (idx - 1) rest
[@@verocaml.spec] [@@verocaml.opaque]

let wrong_result (idx : Vstd.Int.t) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Other]
  | Other -> ()
  | Node _ -> ()
[@@verocaml.proof]

let reachable_false (idx : Vstd.Int.t) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      let _symbolic = idx in
      [%verocaml.assert false]
  | Other -> ()
  | Node _ -> ()
[@@verocaml.proof]

let symbolic_expected
    (idx : Vstd.Int.t)
    (nodes : int node [@finite])
    (expected : int node) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = expected]
  | Other -> ()
  | Node _ -> ()
[@@verocaml.proof]
