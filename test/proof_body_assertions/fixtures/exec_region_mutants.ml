type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let assertion_removed (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  push_front value stack

let reveal_only (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    ()];
  push_front value stack

let reveal_after (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.assert stack_wf stack];
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    ()];
  push_front value stack

let wrong_predicate (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert spec_node_len stack.top = 1];
    ()];
  push_front value stack

let sibling_reveal (choose : bool) (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    if choose then
      [%verocaml.reveal_with_fuel (spec_node_len, 1)]
    else ();
    [%verocaml.assert stack_wf stack];
    ()];
  push_front value stack

let later_block (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    ()];
  [%verocaml.proof
    [%verocaml.assert stack_wf stack];
    ()];
  push_front value stack

let one_branch_export (choose : bool) (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  if choose then
    ([%verocaml.proof
       [%verocaml.reveal_with_fuel (spec_node_len, 1)];
       [%verocaml.assert stack_wf stack];
       ()];
     ())
  else ();
  push_front value stack
