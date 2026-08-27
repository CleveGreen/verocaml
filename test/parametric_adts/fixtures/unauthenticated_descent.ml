type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
let rec size (tree : 'a tree) =
  [%verocaml.decreases tree];
  match tree with Leaf -> 0 | Node _ -> size tree
