type 'a node = Empty | Node of 'a node * 'a * 'a node

let spec_is_empty (tree : int node) : bool =
  match tree with Empty -> true | Node _ -> false
[@@verocaml.spec]

let rec spec_node_eq (left : int node) (right : int node) : bool =
  [%verocaml.decreases left];
  match left with
  | Empty -> spec_is_empty right
  | Node (left_child, left_value, left_rest) ->
      (match right with
      | Empty -> false
      | Node (right_child, right_value, right_rest) ->
          left_value = right_value
          && spec_node_eq left_child right_child
          && spec_node_eq left_rest right_rest)
[@@verocaml.spec] [@@verocaml.revealed]
