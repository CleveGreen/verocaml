type 'a box = { value : 'a }
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let id value =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = value];
  value
let labelled ~first ~second ~flag = if flag then first else second
let pick fallback ?(value = fallback) () = value

let copy_option (value : 'a option) =
  match value with None -> None | Some payload -> Some payload

let copy_result (value : ('a, 'b) result) =
  match value with Ok payload -> Ok payload | Error error -> Error error

let safe_succ value =
  if value = 4611686018427387903 then value else value + 1

let rec length (values : 'a list) =
  [%verocaml.decreases values];
  match values with [] -> 0 | _ :: rest -> safe_succ (length rest)

let rec append (left : 'a list) (right : 'a list) =
  [%verocaml.decreases left];
  match left with [] -> right | value :: rest -> value :: append rest right

let make_box value = { value }
let pass_box box = box
let pass_tree tree = tree

let rec tree_size (tree : 'a tree) =
  [%verocaml.decreases tree];
  match tree with
  | Leaf -> 0
  | Node (_, left, _) -> safe_succ (tree_size left)

let same (_value : 'a) = true
[@@verocaml.spec]

let observe (_value : 'a) = ()
[@@verocaml.proof]
