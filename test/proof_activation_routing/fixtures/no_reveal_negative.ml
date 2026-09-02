type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let lemma_empty_stack (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result -> stack_wf stack];
  ()
[@@verocaml.proof]
