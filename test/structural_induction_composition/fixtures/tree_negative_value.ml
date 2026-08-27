type tree = Leaf of int | Branch of tree * tree

let rec sum (tree : tree) : int =
  [%verocaml.decreases tree];
  match tree with
  | Leaf value -> value
  | Branch (left, right) -> sum left + sum right
[@@verocaml.spec] [@@verocaml.revealed]

let negative_leaf_has_nonnegative_sum (_witness : int) : unit =
  [%verocaml.ensures fun _result ->
    match Leaf (-1) with
    | Leaf value -> value >= 0
    | Branch _ -> true];
  [%verocaml.ensures fun _result -> sum (Leaf (-1)) >= 0];
  ()
[@@verocaml.proof]
