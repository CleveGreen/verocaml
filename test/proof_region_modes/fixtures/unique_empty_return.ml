type node = Empty | Node of int * node

type stack = {
  top : node;
  length : int;
}

let stack_wf (stack : stack) : bool = stack.length >= 0
[@@verocaml.spec]

let lemma_empty_stack (stack : stack) : unit =
  [%verocaml.requires stack_wf stack];
  ()
[@@verocaml.proof]

let empty_stack (_ : unit) : stack =
  [%verocaml.ensures fun result -> stack_wf result];
  let empty = { top = Empty; length = 0 } in
  [%verocaml.proof lemma_empty_stack empty];
  empty
[@@verocaml.external_body]
