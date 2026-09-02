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

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let create_stack_with (value : int) : stack =
  let stack = { top = Empty; length = 0 } in
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert stack_wf stack];
    ()];
  push_front value stack

let scalar_after_let (value : int) : int =
  let local = value in
  [%verocaml.proof
    [%verocaml.assert local = value];
    ()];
  local

let two_assertions () : int =
  [%verocaml.proof
    [%verocaml.reveal_with_fuel (spec_node_len, 1)];
    [%verocaml.assert spec_node_len Empty = 0];
    [%verocaml.reveal_with_fuel (spec_node_len, 2)];
    [%verocaml.assert spec_node_len Empty = 0];
    ()];
  0

let branch_intersection (choose : bool) (value : int) : int =
  let local = value in
  if choose then
    ([%verocaml.proof
       [%verocaml.assert local = value];
       ()];
     ())
  else
    ([%verocaml.proof
       [%verocaml.assert local = value];
       ()];
     ());
  [%verocaml.proof
    [%verocaml.assert local = value];
    ()];
  local
