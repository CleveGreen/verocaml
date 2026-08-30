type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) result
[@@verocaml.external_type_specification]

type 'a batch = { ready : 'a list; deferred : 'a list }
type ('a, 'error) outcome = Accepted of 'a | Rejected of 'error
type 'a tree = Leaf | Branch of 'a * 'a tree * 'a tree

val identity : 'a -> 'a
val choose : bool -> 'a -> 'a -> 'a
val copy_option : 'a option -> 'a option
val copy_result : ('a, 'error) result -> ('a, 'error) result
val copy_outcome : ('a, 'error) outcome -> ('a, 'error) outcome
val append : 'a list -> 'a list -> 'a list
val reverse : 'a list -> 'a list
val length : 'a list -> int
val make_batch : 'a list -> 'a list -> 'a batch
val normalize_batch : 'a batch -> 'a batch
val tree_size : 'a tree -> int
val mirror : 'a tree -> 'a tree
