type 'a tree = Leaf | Node of 'a * 'a tree
let rec left (tree : 'a tree) =
  [%verocaml.decreases tree];
  match tree with Leaf -> 0 | Node (_, rest) -> right rest
and right (tree : 'a tree) =
  [%verocaml.decreases tree];
  match tree with Leaf -> 0 | Node (_, rest) -> left rest
