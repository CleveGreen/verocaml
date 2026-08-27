type 'a nest = Leaf | Node of ('a * 'a) nest

let rec depth : 'a. 'a nest -> int =
 fun xs ->
  [%verocaml.decreases xs];
  match xs with Leaf -> 0 | Node tail -> 1 + depth tail
[@@verocaml.spec] [@@verocaml.revealed]

let instantiate (xs : int nest) : int = depth xs
[@@verocaml.spec]
