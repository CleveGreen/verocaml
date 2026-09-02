type node = Empty | Node of int * node

let rec spec_repeat (value : int) (n : Vstd.Int.t) (node : node) : node =
  [%verocaml.decreases n];
  if n <= 0 then node
  else spec_repeat value (n - 1) (Node (value, node))
[@@verocaml.spec]
[@@verocaml.opaque]

let rec spec_size (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_size next
[@@verocaml.spec]
[@@verocaml.revealed]

let rank_route (value : int) (n : int) (node : node [@finite]) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result ->
    let expected = spec_repeat value n node in
    spec_size expected = spec_size expected];
  ()
