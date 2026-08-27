type 'a node = Empty | Node of 'a * 'a node
type stack = { top : int node }

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rejected (stack : stack) =
  [%verocaml.proof
    let alias = stack.top in
    let _ = spec_node_len alias in
    ()]
