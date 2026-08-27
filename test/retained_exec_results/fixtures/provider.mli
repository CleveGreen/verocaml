type record_result = { value : int; unstated : int }
type variant_result = Missing | Present of int
type 'a box = { box_value : 'a }
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
type chain = End | Link of int * chain

val make_record : int -> record_result
val make_unique_record : int -> record_result @ unique
val make_variant : int -> variant_result
val make_box : 'a -> 'a box
val pass_box : 'a box -> 'a box
val make_tree : 'a -> 'a tree
val pass_tree : 'a tree -> 'a tree
val make_chain : int -> chain
val make_tuple : int -> int * record_result * int box
val pass_record : record_result -> record_result
val uncontracted_record : int -> record_result
val proof_record : int -> record_result
[@@verocaml.proof]
val proof_box : 'a -> 'a box [@@verocaml.proof]
val spec_record : int -> record_result
[@@verocaml.spec]
val spec_box : 'a -> 'a box [@@verocaml.spec]
