type 'a rank_list =
  | Nil
  | Cons of 'a * 'a rank_list

type 'a rank_tree =
  | Leaf
  | Branch of 'a rank_tree * 'a * 'a rank_tree

type 'a container =
  | Empty
  | Pack of ('a * int)

type 'a container_alias = 'a container

type wrapped = Wrap of wrapped container_alias
