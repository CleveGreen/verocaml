type tree = Leaf | Branch of int * tree

let rec spec_repeat (value : int) (n : int) (tree : tree) : tree =
  [%verocaml.decreases n];
  if n <= 0 then tree
  else spec_repeat value (n - 1) (Branch (value, tree))
[@@verocaml.spec]
[@@verocaml.opaque]

let rec spec_tree_size (tree : tree) : int =
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> 0
  | Branch (_, next) -> 1 + spec_tree_size next
[@@verocaml.spec]
[@@verocaml.revealed]

let consume_variant_result
    (value : int)
    (n : int)
    (tree : tree [@finite]) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result ->
    let expected = spec_repeat value n tree in
    spec_tree_size expected = spec_tree_size expected];
  ()
