type 'a node = Empty | Node of 'a * 'a node

type stack = {
  top : int node;
  length : Vstd.Int.t;
}

let spec_push_front (value : int) (stack : stack) : stack =
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }
[@@verocaml.spec]

let rec spec_push_n (value : int) (n : Vstd.Int.t) (stack : stack) : stack =
  [%verocaml.decreases n];
  if n <= 0 then
    stack
  else
    spec_push_n value (n - 1) (spec_push_front value stack)
[@@verocaml.spec]
[@@verocaml.opaque]

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec]
[@@verocaml.revealed]

let consume_recursive_result
    (value : int)
    (n : Vstd.Int.t)
    (stack : stack [@finite]) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result ->
    let expected = spec_push_n value n stack in
    expected.length = expected.length
    && spec_node_len expected.top = spec_node_len expected.top];
  ()
[@@verocaml.proof]
