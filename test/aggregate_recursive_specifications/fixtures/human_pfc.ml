type 'a node = Empty | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let spec_push_front (value : int) (top : int node) : int node =
  Node (value, top)
[@@verocaml.spec]

let rec spec_push_n (value : int) (n : Vstd.Int.t) (top : int node) : int node =
  [%verocaml.decreases n];
  if n <= 0 then
    top
  else
    spec_push_n value (n - 1) (spec_push_front value top)
[@@verocaml.spec]
[@@verocaml.opaque]

let rec spec_node_len (node : int node) : Vstd.Int.t =
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

let rec node_eq_exact
    (left : int node [@finite])
    (right : int node [@finite]) : unit =
  [%verocaml.requires spec_node_eq left right];
  [%verocaml.ensures fun _result -> left = right];
  [%verocaml.decreases left];
  match left, right with
  | Empty, Empty -> ()
  | Node (_, left_next), Node (_, right_next) ->
      node_eq_exact left_next right_next
  | _, _ -> assert false
[@@verocaml.proof]

let did_push_front
    (value : int)
    (old_stack : stack)
    (new_stack : stack) : bool =
  let expected = spec_push_front value old_stack.top in
  new_stack.length = old_stack.length + 1
  && spec_node_eq new_stack.top expected
[@@verocaml.spec]

let did_push_n
    (value : int)
    (n : int)
    (old_stack : stack)
    (new_stack : stack) : bool =
  let expected = spec_push_n value n old_stack.top in
  new_stack.length = old_stack.length + n
  && spec_node_eq new_stack.top expected
[@@verocaml.spec]

let lemma_spec_push_n_step
    (value : int)
    (n : int)
    (top : int node [@finite]) : unit =
  [%verocaml.requires n > 0];
  [%verocaml.ensures fun _result ->
    spec_push_n value n top =
    spec_push_n value (n - 1) (spec_push_front value top)];
  [%verocaml.reveal_with_fuel (spec_push_n, 1)]
[@@verocaml.proof]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.ensures fun result -> did_push_front value stack result];
  let { top; length } = stack in
  [%verocaml.proof node_eq_refl top];
  { top = Node (value, top); length = length + 1 }

let rec push_n
    (value : int)
    (n : int)
    (stack : stack [@finite]) : stack =
  [%verocaml.requires n >= 0];
  [%verocaml.requires stack.length <= 4611686018427387903 - n];
  [%verocaml.ensures fun result -> did_push_n value n stack result];
  [%verocaml.decreases n];
  let { top = _top; length = _length } = stack in
  if n <= 0 then (
    [%verocaml.proof node_eq_refl _top];
    stack
  ) else
    let first = push_front value stack in
    [%verocaml.proof
      node_eq_exact first.top (spec_push_front value stack.top)];
    [%verocaml.proof lemma_spec_push_n_step value n stack.top];
    push_n value (n - 1) first

let checked_push_n_consumer
    (value : int)
    (n : int)
    (stack : stack [@finite]) : stack =
  [%verocaml.requires n >= 0];
  [%verocaml.requires stack.length <= 4611686018427387903 - n];
  [%verocaml.ensures fun result -> did_push_n value n stack result];
  push_n value n stack
