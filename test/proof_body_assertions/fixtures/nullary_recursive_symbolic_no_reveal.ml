type 'a node = Empty | Node of 'a * 'a node

let rec spec_index_alt_imp (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp (idx - 1) rest
[@@verocaml.spec] [@@verocaml.opaque]

let rec spec_other (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty) else spec_other (idx - 1) rest
[@@verocaml.spec] [@@verocaml.opaque]

let rec spec_revealed_default (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_revealed_default (idx - 1) rest
[@@verocaml.spec] [@@verocaml.revealed]

let removed_reveal (idx : int) (nodes : int node [@finite]) =
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]

let late_reveal (idx : int) (nodes : int node [@finite]) =
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Empty];
      [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)]
  | Node _ -> ()
[@@verocaml.proof]

let sibling_reveal
    (take_left : bool)
    (idx : int)
    (nodes : int node [@finite]) =
  if take_left then
    [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)]
  else ();
  if take_left then ()
  else
    match nodes with
    | Empty ->
        [%verocaml.assert spec_index_alt_imp idx nodes = Empty]
    | Node _ -> ()
[@@verocaml.proof]

let wrong_callee_reveal (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_other, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]

let synthetic_only (idx : int) (nodes : int node [@finite]) =
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]

let zero_fuel (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 0)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]

let revealed_default (idx : int) (nodes : int node [@finite]) =
  match nodes with
  | Empty ->
      [%verocaml.assert spec_revealed_default idx nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]
