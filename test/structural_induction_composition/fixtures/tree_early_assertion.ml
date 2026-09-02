type tree = Leaf of int | Branch of tree * tree

let rec sum (tree : tree) : Vstd.Int.t =
  [%verocaml.decreases tree];
  match tree with
  | Leaf value -> value
  | Branch (left, right) -> sum left + sum right
[@@verocaml.spec] [@@verocaml.revealed]

let rec all_nonnegative (tree : tree) : bool =
  [%verocaml.decreases tree];
  match tree with
  | Leaf value -> value >= 0
  | Branch (left, right) ->
      all_nonnegative left && all_nonnegative right
[@@verocaml.spec] [@@verocaml.revealed]

let rec nonnegative_sum (tree : tree [@finite]) : unit =
  [%verocaml.assert
    not (all_nonnegative tree) || sum tree >= 0];
  [%verocaml.ensures fun _result ->
    not (all_nonnegative tree) || sum tree >= 0];
  [%verocaml.decreases tree];
  match tree with
  | Leaf _ -> ()
  | Branch (left, right) ->
      nonnegative_sum left;
      nonnegative_sum right
[@@verocaml.proof]
