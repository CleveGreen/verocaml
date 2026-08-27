type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let same left right = left = right [@@verocaml.spec]

let false_identity (value : 'a tree) : unit =
  [%verocaml.assert same value Leaf];
  ()
[@@verocaml.proof]
