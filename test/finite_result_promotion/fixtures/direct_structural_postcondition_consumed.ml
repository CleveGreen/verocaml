type node = Empty | Node of int * node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let rec rebuild (node : node [@finite]) : node =
  [%verocaml.ensures fun result ->
    spec_node_len result = spec_node_len node];
  [%verocaml.decreases node];
  match node with
  | Empty -> Empty
  | Node (value, next) -> Node (value, rebuild next)

let consume (_node : node [@finite]) : unit = ()

let use (node : node [@finite]) : unit = consume (rebuild node)
