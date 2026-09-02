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
[@@verocaml.spec]
[@@verocaml.opaque]

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
[@@verocaml.opaque]

let did_push_front (value : int) (old : stack) (cur : stack) : bool =
  let expected = spec_push_front value old in
  cur.length = expected.length && spec_node_eq cur.top expected.top
[@@verocaml.spec]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.ensures fun result -> did_push_front value stack result];
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }
