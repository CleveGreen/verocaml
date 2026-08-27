type node = Empty | Node of int * node

let rec spec_repeat (value : int) (n : int) (node : node) : node =
  [%verocaml.decreases n];
  if n <= 0 then node
  else spec_repeat value (n - 1) (Node (value, node))
[@@verocaml.spec]
[@@verocaml.opaque]

let rec spec_observe (node : node) : bool =
  [%verocaml.decreases node];
  match node with
  | Empty -> true
  | Node (_, next) -> spec_observe next
[@@verocaml.spec]
[@@verocaml.revealed]

let recursive_argument_route
    (value : int)
    (n : int)
    (node : node [@finite]) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result ->
    let expected = spec_repeat value n node in
    spec_observe expected = spec_observe expected];
  ()
