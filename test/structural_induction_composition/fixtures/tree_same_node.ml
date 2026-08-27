type tree = Leaf of int | Branch of tree * tree

let rec nonnegative_sum (tree : tree [@finite]) : unit =
  [%verocaml.ensures fun _result -> true];
  [%verocaml.decreases tree];
  match tree with Leaf _ -> () | Branch _ -> nonnegative_sum tree
[@@verocaml.proof]
