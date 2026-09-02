type 'a node = Empty | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let identity_node (node : int node) : int node = node
[@@verocaml.spec]

let identity_stack (stack : stack) : stack = stack
[@@verocaml.spec]

let spec_push_front (value : int) (stack : stack) : int node =
  Node (value, stack.top)
[@@verocaml.spec]

let spec_front (value : int) (stack : stack) : int =
  match identity_node (spec_push_front value stack) with
  | Empty -> 0
  | Node (head, _) -> head
[@@verocaml.spec]

let verified (value : int) (stack : stack) =
  [%verocaml.assert spec_front value stack = value];
  let[@tracked] tracked = (stack [@tracked]) in
  let[@ghost] ghost = (stack [@ghost]) in
  [%verocaml.proof
    let _ = identity_stack stack in
    let _ = identity_stack tracked in
    let _ = identity_stack ghost in
    ()];
  value

let node_equal (left : int node) (right : int node) : bool =
  left = right
[@@verocaml.spec]

let nested_identity (pair : stack * int node) : stack * int node =
  let stack, node = pair in
  (stack, node)
[@@verocaml.spec]
