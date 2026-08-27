type node = Empty | Node of int * node

let rec spec_node_len (node : node) : int =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let rec rebuild (node : node [@finite]) : node =
  [%verocaml.decreases node];
  match node with
  | Empty -> Empty
  | Node (value, next) -> Node (value, rebuild next)

let consume (node : node [@finite]) : unit =
  [%verocaml.ensures fun _result -> spec_node_len node = spec_node_len node];
  ()
[@@verocaml.proof]

let use (node : node [@finite]) : unit =
  let result = rebuild node in
  [%verocaml.proof consume result];
  ()
