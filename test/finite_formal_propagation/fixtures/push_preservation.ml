type node = Empty | Node of int * node

type stack = {
  top : node;
  length : int;
}

let rec spec_node_len (value : node) : int =
  [%verocaml.decreases value];
  match value with
  | Empty -> 0
  | Node (_, tail) -> 1 + abs (spec_node_len tail)
[@@verocaml.spec] [@@verocaml.revealed]

let stack_wf (stack : stack [@finite]) : bool =
  spec_node_len stack.top >= 0
[@@verocaml.spec]

let spec_push_front (value : int) (stack : stack) : stack =
  { top = Node (value, stack.top); length = stack.length + 1 }
[@@verocaml.spec]

let lemma_push_front_wf (stack : stack [@finite]) : unit =
  [%verocaml.assert
    let pushed =
      { top = Node (1, stack.top); length = stack.length + 1 }
    in
    stack_wf pushed];
  ()
[@@verocaml.proof]
