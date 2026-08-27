type 'a chain = Empty | Link of 'a * 'a chain
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
let rec bad (xs : int chain) (tree : int tree) : int =
  [%verocaml.decreases xs];
  match tree with Leaf _ -> 0 | Node (left, _) -> bad xs left
[@@verocaml.spec] [@@verocaml.opaque]
