type 'a node = Empty | Node of 'a * 'a node
type stack = { top : int node }

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rejected choose (stack : stack) =
  let selected = if choose then stack.top else stack.top in
  [%verocaml.proof
    let _ = spec_node_len selected in
    ()]
