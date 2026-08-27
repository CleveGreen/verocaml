[@@@verocaml.verify]

type record_result = { value : int; unstated : int }
type variant_result = Missing | Present of int
type 'a box = { box_value : 'a }
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
type chain = End | Link of int * chain

let make_record value =
  [%verocaml.ensures fun result -> result.value = value];
  { value; unstated = 41 }

let make_unique_record value : record_result @ unique =
  [%verocaml.ensures fun result -> result.value = value];
  { value; unstated = 42 }

let make_variant value = if value = 0 then Missing else Present value
let make_box (box_value : 'a) : 'a box = { box_value }
let pass_box (box : 'a box) : 'a box = box
let make_tree (item : 'a) : 'a tree = Node (item, Leaf, Leaf)
let pass_tree (tree : 'a tree) : 'a tree = tree

let rec make_chain count =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then End else Link (count, make_chain (count - 1))

let make_tuple value =
  (value, { value; unstated = 43 }, { box_value = value })

let pass_record record = record
let uncontracted_record value = { value; unstated = 44 }

let proof_record value = { value; unstated = value }
[@@verocaml.proof]

let proof_box box_value = { box_value } [@@verocaml.proof]

let spec_record value = { value; unstated = value }
[@@verocaml.spec]

let spec_box box_value = { box_value } [@@verocaml.spec]
