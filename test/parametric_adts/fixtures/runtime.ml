type 'a box = Box of 'a
type 'a pair = { left : 'a; right : 'a }
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
let copy_option = function None -> None | Some value -> Some value
let copy_result = function Ok value -> Ok value | Error error -> Error error
let copy_box (Box value) = Box value
let copy_pair pair = { left = pair.left; right = pair.right }
let rec length = function [] -> 0 | _ :: rest -> 1 + length rest
let rec append left right = match left with [] -> right | value :: rest -> value :: append rest right
let rec tree_size = function Leaf -> 0 | Node (_, left, right) -> 1 + tree_size left + tree_size right
let rec tree_height = function Leaf -> 0 | Node (_, left, right) -> 1 + max (tree_height left) (tree_height right)
let () =
  let tree = Node (1, Node (2, Leaf, Leaf), Leaf) in
  assert (copy_option (Some 3) = Some 3);
  assert (copy_result (Error true) = Error true);
  assert (copy_box (Box 4) = Box 4);
  assert ((copy_pair { left = 5; right = 6 }).right = 6);
  assert (length [1; 2; 3] = 3);
  assert (append [1; 2] [3] = [1; 2; 3]);
  assert (tree_size tree = 2);
  assert (tree_height tree = 2);
  print_endline "parametric-adts-runtime: ok"
