type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
type 'a pair = { left : 'a; right : 'a }

let same left right = left = right [@@verocaml.spec]

let tree_reflexive (value : 'a tree) : unit =
  [%verocaml.assert same value value];
  ()
[@@verocaml.proof]

let rebuild_tree (value : 'a tree) =
  match value with
  | Leaf -> Leaf
  | Node (payload, left, right) ->
      Node (payload, left, right)
[@@verocaml.spec]

let tree_reconstruction (value : 'a tree) : unit =
  [%verocaml.assert same value (rebuild_tree value)];
  ()
[@@verocaml.proof]

let record_reconstruction (value : 'a pair) : unit =
  [%verocaml.assert same value { left = value.left; right = value.right }];
  ()
[@@verocaml.proof]
