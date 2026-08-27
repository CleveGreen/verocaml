type 'a node = Empty | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let spec_push_front (value : int) (stack : stack) : stack =
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }
[@@verocaml.spec]

let rec spec_push_n (value : int) (n : int) (stack : stack) : stack =
  [%verocaml.decreases n];
  if n <= 0 then
    stack
  else
    spec_push_n value (n - 1) (spec_push_front value stack)
[@@verocaml.spec]
[@@verocaml.opaque]

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec]
[@@verocaml.revealed]

let rec spec_node_eq (left : int node) (right : int node) : bool =
  [%verocaml.decreases left];
  match left with
  | Empty -> (match right with Empty -> true | Node _ -> false)
  | Node (left_value, left_next) ->
      (match right with
       | Empty -> false
       | Node (right_value, right_next) ->
           left_value = right_value
           && spec_node_eq left_next right_next)
[@@verocaml.spec]
[@@verocaml.revealed]

let rec node_eq_refl (node : int node [@finite]) : unit =
  [%verocaml.ensures fun _result -> spec_node_eq node node];
  [%verocaml.decreases node];
  match node with
  | Empty -> ()
  | Node (_, next) -> node_eq_refl next
[@@verocaml.proof]

let did_push_front
    (value : int)
    (old_stack : stack)
    (new_stack : stack) : bool =
  let expected = spec_push_front value old_stack in
  new_stack.length = expected.length
  && spec_node_eq new_stack.top expected.top
[@@verocaml.spec]

let did_push_n
    (value : int)
    (n : int)
    (old_stack : stack)
    (new_stack : stack) : bool =
  let expected = spec_push_n value n old_stack in
  new_stack.length = expected.length
  && spec_node_eq new_stack.top expected.top
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.ensures fun result ->
    let { top; length } = stack in
    result = { top = Node (value, top); length = length + 1 }];
  [%verocaml.ensures fun result -> result.length = stack.length + 1];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }

let rec push_n
    (value : int)
    (n : int)
    (stack : stack [@finite]) : stack =
  [%verocaml.requires n >= 0];
  [%verocaml.requires stack.length <= 4611686018427387903 - n];
  [%verocaml.ensures fun result -> did_push_n value n stack result];
  [%verocaml.decreases n];
  let { top; length = _length } = stack in
  if n <= 0 then (
    [%verocaml.proof node_eq_refl top];
    stack
  ) else
    let first = push_front value stack in
    push_n value (n - 1) first

let checked_push_n_consumer
    (value : int)
    (n : int)
    (stack : stack [@finite]) : stack =
  [%verocaml.requires n >= 0];
  [%verocaml.requires stack.length <= 4611686018427387903 - n];
  [%verocaml.ensures fun result -> did_push_n value n stack result];
  push_n value n stack
