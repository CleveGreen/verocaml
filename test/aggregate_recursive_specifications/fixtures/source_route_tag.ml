type node = Empty | Node of int * node

let rec spec_repeat (value : int) (n : int) (node : node) : node =
  [%verocaml.decreases n];
  if n <= 0 then node
  else spec_repeat value (n - 1) (Node (value, node))
[@@verocaml.spec]
[@@verocaml.opaque]

let tag_route (value : int) (n : int) (node : node [@finite]) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result ->
    match spec_repeat value n node with
    | Empty -> true
    | Node _ -> true];
  ()
