type 'a node = Empty | Node of 'a * 'a node
type box = { value : int }

let spec_is_empty (value : int node) : bool =
  match value with Empty -> true | Node _ -> false
[@@verocaml.spec]

let rec spec_index_alt_imp (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp (idx - 1) rest
[@@verocaml.spec] [@@verocaml.opaque]

let rec spec_payload_result (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Node (idx, Empty)
  | Node (_, rest) -> spec_payload_result idx rest
[@@verocaml.spec] [@@verocaml.opaque]

let rec spec_record_result (idx : int) (nodes : int node) : box =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> { value = idx }
  | Node (_, rest) -> spec_record_result idx rest
[@@verocaml.spec] [@@verocaml.opaque]

let rec spec_guarded (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty when idx >= 0 -> Empty
  | Empty -> Empty
  | Node (_, rest) -> spec_guarded idx rest
[@@verocaml.spec] [@@verocaml.opaque]

let rec spec_count (nodes : int node) : int =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> 0
  | Node (_, rest) -> 1 + spec_count rest
[@@verocaml.spec] [@@verocaml.opaque]

let rec spec_recursive_branch
    (idx : int)
    (nodes : int node) : int node =
  [%verocaml.decreases idx];
  match nodes with
  | Empty ->
      if idx <= 0 then Empty
      else spec_recursive_branch (idx - 1) nodes
  | Node _ -> Empty
[@@verocaml.spec] [@@verocaml.opaque]

let non_nullary_input (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty -> ()
  | Node _ ->
      [%verocaml.assert spec_index_alt_imp idx nodes = Empty]
[@@verocaml.proof]

let payload_result (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_payload_result, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_payload_result idx nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]

let record_result (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_record_result, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert (spec_record_result idx nodes).value = idx]
  | Node _ -> ()
[@@verocaml.proof]

let selector_actual
    (parameters : box)
    (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert
        spec_index_alt_imp parameters.value nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]

let nested_application (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_is_empty (spec_index_alt_imp idx nodes)]
  | Node _ -> ()
[@@verocaml.proof]

let guarded_branch (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_guarded, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_guarded idx nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]

let multiple_applications (idx : int) (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert
        spec_index_alt_imp idx nodes = spec_index_alt_imp (idx + 1) nodes]
  | Node _ -> ()
[@@verocaml.proof]

let scalar_recursive (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_count, 2)];
  match nodes with
  | Empty -> [%verocaml.assert spec_count nodes = 0]
  | Node _ -> ()
[@@verocaml.proof]

let recursive_scalar_actual (idx : int) (nodes : int node [@finite]) =
  let _symbolic = idx in
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  [%verocaml.reveal_with_fuel (spec_count, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert
        spec_index_alt_imp (spec_count nodes) nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]

let recursive_selected_branch
    (idx : int)
    (nodes : int node [@finite]) =
  [%verocaml.reveal_with_fuel (spec_recursive_branch, 2)];
  match nodes with
  | Empty ->
      [%verocaml.assert spec_recursive_branch idx nodes = Empty]
  | Node _ -> ()
[@@verocaml.proof]
