type node = Empty | Node of int * node

let rec rebuild (node : node [@finite]) : node =
  [%verocaml.decreases node];
  match node with
  | Empty -> Empty
  | Node (value, _next) -> Node (value, rebuild node)

let consume (_node : node [@finite]) : unit = ()

let use (node : node [@finite]) = consume (rebuild node)
