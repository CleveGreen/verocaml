[@@@verocaml.verify]

type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) result
[@@verocaml.external_type_specification]

type 'a box = Box of 'a

type 'a pair = { left : 'a; right : 'a }

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let copy_option (value : 'a option) : 'a option =
  match value with None -> None | Some payload -> Some payload

let copy_result (value : ('a, 'b) result) : ('a, 'b) result =
  match value with Ok payload -> Ok payload | Error error -> Error error

let copy_box (box : 'a box) : 'a box = match box with Box value -> Box value
let make_pair value : 'a pair = { left = value; right = value }
let copy_pair (pair : 'a pair) : 'a pair =
  { left = pair.left; right = pair.right }

let safe_add (left : int) (right : int) : int =
  [%verocaml.requires left >= 0];
  [%verocaml.requires right >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  if left > 4611686018427387903 - right then 4611686018427387903 else left + right

let safe_succ value =
  [%verocaml.requires value >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  safe_add 1 value

let rec length (values : 'a list) : int =
  [%verocaml.ensures fun result -> result >= 0];
  [%verocaml.decreases values];
  match values with [] -> 0 | _ :: rest -> safe_succ (length rest)

let rec append (left : 'a list) (right : 'a list) : 'a list =
  [%verocaml.decreases left];
  match left with [] -> right | value :: rest -> value :: append rest right

let rec tree_size (tree : 'a tree) : int =
  [%verocaml.ensures fun result -> result >= 0];
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> 0
  | Node (_, left, right) -> safe_succ (safe_add (tree_size left) (tree_size right))

let max (left : int) (right : int) : int =
  [%verocaml.requires left >= 0];
  [%verocaml.requires right >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  if left >= right then left else right

let rec tree_height (tree : 'a tree) : int =
  [%verocaml.ensures fun result -> result >= 0];
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> 0
  | Node (_, left, right) -> safe_succ (max (tree_height left) (tree_height right))
