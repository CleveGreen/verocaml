type int_list =
  | Nil
  | Cons of int * int_list

type tree =
  | Leaf
  | Branch of tree * int * tree
