type node = Empty | Node of int * node

let rec spec_node_len (node : node) : int =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let choose flag (xs : node [@finite]) (arbitrary : node) : node =
  if flag then Node (1, xs) else arbitrary

let consume (xs : node [@finite]) : unit =
  [%verocaml.assert spec_node_len xs >= 0];
  ()
[@@verocaml.proof]

let use flag (xs : node [@finite]) arbitrary : int =
  let result = choose flag xs arbitrary in
  [%verocaml.proof consume result];
  0
