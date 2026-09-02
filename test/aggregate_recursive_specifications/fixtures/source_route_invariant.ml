type node = Empty | Node of int * node

let node_invariant (node : node) : bool =
  match node with Empty -> true | Node _ -> true
[@@verocaml.type_invariant]

let rec spec_repeat (value : int) (n : Vstd.Int.t) (node : node) : node =
  [%verocaml.decreases n];
  if n <= 0 then node
  else spec_repeat value (n - 1) (Node (value, node))
[@@verocaml.spec]
[@@verocaml.opaque]

let invariant_route (value : int) (n : int) (node : node [@finite]) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result ->
    node_invariant (spec_repeat value n node)];
  ()
