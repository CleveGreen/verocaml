type 'a node = Empty | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let spec_push_front (value : int) (stack : stack) : stack =
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }
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
  let expected = spec_push_front value old in
  cur.length = expected.length && spec_node_eq cur.top expected.top
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.ensures fun result -> did_push_front value stack result];
  let { top; length } = stack in
  let result = { top = Node (value, top); length = length + 1 } in
  [%verocaml.proof node_eq_refl top];
  result

let did_push_front_twice (a : int) (b : int) (old : stack) (cur : stack) : bool =
  did_push_front b (spec_push_front a old) cur
[@@verocaml.spec]

let push_two_to_front (a : int) (b : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387902];
  [%verocaml.ensures fun result -> did_push_front_twice a b stack result];
  let first = push_front a stack in
  let second = push_front b first in
  [%verocaml.proof
    let { top; length = _stack_length } = stack in
    let { top = first_top; length = _first_length } = first in
    let { top = second_top; length = _second_length } = second in
    node_eq_trans
      second_top
      (Node (b, first_top))
      (Node (b, Node (a, top)))];
  second

(*
let rec spec_push_n (value : int) (n : int) (stack : stack) : stack =
  [%verocaml.decreases n];
  if n <= 0 then
    stack
  else
    spec_push_n value (n - 1) (spec_push_front value stack)
[@@verocaml.spec]
[@@verocaml.opaque]
*)
(*
let did_push_n
    (value : int)
    (n : int)
    (old : stack)
    (cur : stack) : bool =
  let expected = spec_push_n value n old in
  cur.length = expected.length
  && spec_node_eq cur.top expected.top
[@@verocaml.spec]

let rec push_n
    (value : int)
    (n : int)
    (stack : stack [@finite]) : stack =
  [%verocaml.requires n >= 0];
  [%verocaml.requires stack.length <= 4611686018427387903 - n];
  [%verocaml.ensures fun result -> did_push_n value n stack result];
  [%verocaml.decreases n];

  if n = 0 then begin
    [%verocaml.proof node_eq_refl stack.top];
    stack
  end else begin
    let first = push_front value stack in
    let result = push_n value (n - 1) first in
    result
  end
*)
