type 'a node = Empty | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let spec_push_front (value : int) (stack : stack) : int node =
  Node (value, stack.top)
[@@verocaml.spec]

let rec spec_node_eq (a : int node) (b : int node) : bool =
  [%verocaml.decreases a];
  match a with
  | Empty -> (match b with Empty -> true | Node _ -> false)
  | Node (va, na) ->
      (match b with
       | Node (vb, nb) -> va = vb && spec_node_eq na nb
       | Empty -> false)
[@@verocaml.spec]
[@@verocaml.revealed]

let rec node_eq_refl (node : int node [@finite]) : unit =
  [%verocaml.ensures fun _result -> spec_node_eq node node];
  [%verocaml.decreases node];
  match node with
  | Empty -> ()
  | Node (_, next) -> node_eq_refl next
[@@verocaml.proof]

let rec node_eq_trans
    (a : int node [@finite])
    (b : int node [@finite])
    (c : int node [@finite]) : unit =
  [%verocaml.requires spec_node_eq a b];
  [%verocaml.requires spec_node_eq b c];
  [%verocaml.ensures fun _result -> spec_node_eq a c];
  [%verocaml.decreases a];
  match a with
  | Empty -> ()
  | Node (_, an) ->
      (match b with
       | Empty -> ()
       | Node (_, bn) ->
           (match c with
            | Empty -> ()
            | Node (_, cn) -> node_eq_trans an bn cn))
[@@verocaml.proof]

let did_push_front (value : int) (old : stack) (cur : stack) : bool =
  cur.length = old.length + 1
  && spec_node_eq cur.top (spec_push_front value old)
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.ensures fun result -> did_push_front value stack result];
  let { top; length } = stack in
  let result = { top = Node (value, top); length = length + 1 } in
  [%verocaml.proof node_eq_refl top];
  result

let did_push_front_twice (a : int) (b : int) (old : stack) (cur : stack) : bool =
  cur.length = old.length + 2
  && spec_node_eq cur.top (Node (b, spec_push_front a old))
[@@verocaml.spec]

let push_two_to_front_let
    (a : int) (b : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387902];
  [%verocaml.ensures fun result -> did_push_front_twice a b stack result];
  let { top; length = _stack_length } = stack in
  let first = push_front a stack in
  let { top = first_top; length = _first_length } = first in
  let second = push_front b first in
  let { top = second_top; length = _second_length } = second in
  [%verocaml.proof
    node_eq_trans
      second_top
      (Node (b, first_top))
      (Node (b, Node (a, top)))];
  second
