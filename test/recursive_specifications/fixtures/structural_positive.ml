open Vstd

type 'a chain = Empty | Link of 'a * 'a chain
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let rec length (xs : int chain) : Int.t =
  [%verocaml.decreases xs];
  match xs with Empty -> 0 | Link (_, tail) -> 1 + length tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec equal_chain (xs : int chain) (ys : int chain) : bool =
  [%verocaml.decreases xs];
  if xs = ys then true
  else
    match xs with
    | Empty -> Empty = ys
    | Link (x, xt) ->
        (match ys with
        | Empty -> false
        | Link (y, yt) -> x = y && equal_chain xt yt)
[@@verocaml.spec] [@@verocaml.revealed]

let rec tree_size (tree : int tree) : Int.t =
  [%verocaml.decreases tree];
  match tree with
  | Leaf _ -> 1
  | Node (left, right) -> 1 + tree_size left + tree_size right
[@@verocaml.spec] [@@verocaml.revealed]

let rec chain_induction (xs : bool chain) : unit =
  [%verocaml.decreases xs];
  match xs with Empty -> () | Link (_, tail) -> chain_induction tail
[@@verocaml.proof]
