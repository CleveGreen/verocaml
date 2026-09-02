type node = Empty | Node of int * node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let push_front value (xs : node [@finite]) : node =
  Node (value, xs)

let consume (xs : node [@finite]) : unit =
  [%verocaml.ensures fun _result -> spec_node_len xs = spec_node_len xs];
  ()
[@@verocaml.proof]

let use (xs : node [@finite]) : int =
  let result = push_front 1 xs in
  [%verocaml.proof consume result];
  0
