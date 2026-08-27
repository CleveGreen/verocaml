type tree = Leaf of int | Branch of tree * tree

let rec revisit_after_join (choose : bool) (tree : tree [@finite]) : unit =
  [%verocaml.ensures fun _result -> true];
  [%verocaml.decreases tree];
  match tree with
  | Leaf _ -> ()
  | Branch (left, right) ->
      if choose then revisit_after_join choose left else ();
      revisit_after_join choose left;
      revisit_after_join choose right
[@@verocaml.proof]
