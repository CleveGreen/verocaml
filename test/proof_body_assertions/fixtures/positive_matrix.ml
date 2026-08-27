type node = Empty | Node of int * node

let rec spec_node_len (node : node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let pure_let (x : int) =
  let y = x + 1 in
  [%verocaml.assert y = x + 1]
[@@verocaml.proof]

let branch_reveal (take_left : bool) =
  if take_left then (
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert spec_node_len Empty = 0])
  else ()
[@@verocaml.proof]

let both_predecessors (take_left : bool) =
  let empty = Empty in
  if take_left then
    ([%verocaml.proof
       [%verocaml.reveal_with_fuel (spec_node_len, 1)];
       [%verocaml.assert spec_node_len empty = 0];
       ()];
     ())
  else
    ([%verocaml.proof
       [%verocaml.reveal_with_fuel (spec_node_len, 1)];
       [%verocaml.assert spec_node_len empty = 0];
       ()];
     ());
  [%verocaml.proof
    [%verocaml.assert spec_node_len empty <= 0];
    ()];
  ()

let tracked_local (source : int [@tracked]) =
  let[@tracked] tracked = (source [@tracked]) in
  [%verocaml.assert (tracked [@tracked]) = (source [@tracked])]
[@@verocaml.proof]
