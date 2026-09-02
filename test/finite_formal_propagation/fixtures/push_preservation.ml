type node = Empty | Node of int * node

type stack = {
  top : node;
  length : int;
}

let rec spec_node_len (value : node) : Vstd.Int.t =
  [%verocaml.decreases value];
  match value with
  | Empty -> 0
  | Node (_, tail) -> 1 + abs (spec_node_len tail)
[@@verocaml.spec] [@@verocaml.revealed]

let stack_wf (stack : stack [@finite]) : bool =
  spec_node_len stack.top >= 0
[@@verocaml.spec]

let spec_push_front (value : int) (stack : stack) : node =
  Node (value, stack.top)
[@@verocaml.spec]

let lemma_push_front_wf (stack : stack [@finite]) : unit =
  [%verocaml.assert
    spec_node_len (spec_push_front 1 stack) >= 0];
  ()
[@@verocaml.proof]
