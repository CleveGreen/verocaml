type tree = Leaf of int | Branch of tree * tree

let rec sum (tree : tree) : Vstd.Int.t =
  [%verocaml.decreases tree];
  match tree with
  | Leaf value -> value
  | Branch (left, right) -> sum left + sum right
[@@verocaml.spec] [@@verocaml.revealed]

let negative_leaf_has_nonnegative_sum (witness : int) : unit =
  [%verocaml.requires witness = -1];
  [%verocaml.ensures fun _result ->
    match Leaf witness with
    | Leaf value -> value >= 0
    | Branch _ -> true];
  [%verocaml.ensures fun _result -> sum (Leaf witness) >= 0];
  ()
[@@verocaml.proof]
