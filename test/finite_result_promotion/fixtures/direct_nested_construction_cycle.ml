type node = Empty | Node of int * node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let rec build count (xs : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then Node (0, xs)
  else Node (count, build (count - 1) xs)

let consume (xs : node [@finite]) : unit =
  [%verocaml.ensures fun _result -> spec_node_len xs = spec_node_len xs];
  ()
[@@verocaml.proof]

let use count (xs : node [@finite]) : int =
  [%verocaml.requires count >= 0];
  let result = build count xs in
  [%verocaml.proof consume result];
  0
