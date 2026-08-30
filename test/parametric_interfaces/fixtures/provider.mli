type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) result
[@@verocaml.external_type_specification]

type 'a box = { value : 'a }
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

val id : 'a -> 'a
val labelled : first:'a -> second:'a -> flag:bool -> 'a
val pick : 'a -> ?value:'a -> unit -> 'a
val copy_option : 'a option -> 'a option
val copy_result : ('a, 'b) result -> ('a, 'b) result
val length : 'a list -> int
val append : 'a list -> 'a list -> 'a list
val make_box : 'a -> 'a box
val pass_box : 'a box -> 'a box
val pass_tree : 'a tree -> 'a tree
val tree_size : 'a tree -> int
val same : 'a -> bool
[@@verocaml.spec]
val observe : 'a -> unit
[@@verocaml.proof]
