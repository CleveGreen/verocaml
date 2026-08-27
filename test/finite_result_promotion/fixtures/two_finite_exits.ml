type node = Empty | Node of int * node

let rec spec_node_len (node : node) : int =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let choose flag (xs : node [@finite]) : node =
  if flag then Node (1, xs) else Node (2, xs)

let consume (xs : node [@finite]) : unit =
  [%verocaml.ensures fun _result -> spec_node_len xs = spec_node_len xs];
  ()
[@@verocaml.proof]

let use flag (xs : node [@finite]) : int =
  let result = choose flag xs in
  [%verocaml.proof consume result];
  0
