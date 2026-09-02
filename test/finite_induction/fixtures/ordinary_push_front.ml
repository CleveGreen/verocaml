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

let did_push_front
    (value : int)
    (old : stack [@finite])
    (cur : stack [@finite]) : bool =
  cur.length = old.length + 1
  && spec_node_eq cur.top (spec_push_front value old)
[@@verocaml.spec]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.ensures fun result -> did_push_front value stack result];
  let { top; length } = stack in
  let result = { top = Node (value, top); length = length + 1 } in
  [%verocaml.proof node_eq_refl top];
  result
