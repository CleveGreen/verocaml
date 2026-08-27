type record_result = { value : int; unstated : int }
type 'a box = { box_value : 'a }
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

val id : 'a -> 'a
val copy_option : 'a option -> 'a option
val copy_result : ('a, 'b) result -> ('a, 'b) result
val length : 'a list -> int
val append : 'a list -> 'a list -> 'a list
val tree_size : 'a tree -> int
val make_record : int -> record_result
val make_box : 'a -> 'a box
val pass_box : 'a box -> 'a box
val make_tree : 'a -> 'a tree
val pass_tree : 'a tree -> 'a tree
val make_unique_record : int -> record_result @ unique
