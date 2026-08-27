type 'a node =
  | Empty
  | Node of 'a * 'a node

type 'a option =
  | None
  | Some of 'a

let spec_is_none (value : int option) =
  match value with
  | None -> true
  | Some _ -> false
[@@verocaml.spec]

let spec_opt_eq (left : int option) (right : int option) =
  match left with
  | None -> spec_is_none right
  | Some left_value -> (
      match right with
      | None -> false
      | Some right_value -> left_value = right_value)
[@@verocaml.spec]

let rec spec_index_source (index : int) (nodes : int node) =
  [%verocaml.decreases nodes];
  match nodes with
  | Empty -> Empty
  | Node (value, rest) ->
      if index <= 0 then Node (value, Empty)
      else spec_index_source (index - 1) rest
[@@verocaml.spec]
[@@verocaml.revealed]

let spec_index (index : int) (nodes : int node) =
  match spec_index_source index nodes with
  | Empty -> None
  | Node (value, _) -> Some value
[@@verocaml.spec]

let lemma_opt_eq_none_trans (left : int option) (middle : int option) =
  [%verocaml.requires spec_opt_eq left middle];
  [%verocaml.requires spec_opt_eq middle None];
  [%verocaml.ensures fun _ -> spec_opt_eq left None];
  match left with
  | None -> ()
  | Some _ -> (
      match middle with
      | None -> [%verocaml.assert false]
      | Some _ -> [%verocaml.assert false])
[@@verocaml.proof]

let lemma_index_node_step_none
    (index : int)
    (value : int)
    (rest : int node [@finite]) =
  [%verocaml.requires index > 0];
  [%verocaml.requires
    spec_opt_eq (spec_index (index - 1) rest) None];
  [%verocaml.ensures fun _ ->
    spec_opt_eq (spec_index index (Node (value, rest))) None];
  [%verocaml.reveal_with_fuel (spec_index_source, 1)];
  let tail = spec_index_source (index - 1) rest in
  match tail with
  | Empty -> ()
  | Node (tail_value, _) ->
      [%verocaml.assert
        spec_opt_eq (spec_index (index - 1) rest) (Some tail_value)];
      [%verocaml.assert spec_opt_eq (Some tail_value) None];
      [%verocaml.assert false]
[@@verocaml.proof]

let recursive_helper (index : int) (nodes : int node) =
  spec_index_source index nodes
[@@verocaml.spec]

let helper_hidden_recursive_if
    (condition : bool)
    (index : int)
    (nodes : int node) =
  if condition then recursive_helper index nodes else nodes
[@@verocaml.spec]

let mixed_cache_recursive_match
    (condition : bool)
    (index : int)
    (nodes : int node) =
  let top_level = recursive_helper index nodes in
  match condition with
  | false -> top_level
  | true -> recursive_helper index top_level
[@@verocaml.spec]

let helper_hidden_if_falls_back
    (condition : bool)
    (index : int)
    (nodes : int node [@finite]) =
  [%verocaml.requires index >= 0];
  [%verocaml.ensures fun _ ->
    let observed = helper_hidden_recursive_if condition index nodes in
    observed = observed];
  ()

let mixed_cache_match_falls_back
    (condition : bool)
    (index : int)
    (nodes : int node [@finite]) =
  [%verocaml.requires index >= 0];
  [%verocaml.ensures fun _ ->
    let observed = mixed_cache_recursive_match condition index nodes in
    observed = observed];
  ()
