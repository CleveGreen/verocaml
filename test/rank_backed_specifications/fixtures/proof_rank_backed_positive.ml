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
[@@verocaml.revealed]

let spec_push_front (value : int) (stack : stack) : int node =
  Node (value, stack.top)
[@@verocaml.spec]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let lemma_stack_push_length (value : int) (nodes : int node [@finite]) =
  let pushed = Node (value, nodes) in
  let _ = pushed in
  ()
[@@verocaml.proof]

let lemma_push_front_wf (value : int) (stack : stack [@finite]) =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  lemma_stack_push_length value stack.top;
  let pushed = spec_push_front value stack in
  [%verocaml.assert spec_node_len pushed = spec_node_len pushed];
  ()
[@@verocaml.proof]

let lemma_push_front_wf_tracked
    (value : int)
    (stack : stack [@finite] [@tracked]) =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  lemma_stack_push_length value stack.top;
  let pushed = spec_push_front value stack in
  [%verocaml.assert spec_node_len pushed = spec_node_len pushed];
  ()
[@@verocaml.proof]

let complete_shape (stack : stack [@finite]) =
  let rebuilt = { top = stack.top; length = stack.length } in
  let top =
    match rebuilt with
    | { top; length = _ } -> top
  in
  match top with
  | Empty -> ()
  | Node (_, rest) ->
      let _ = rest in
      ()
[@@verocaml.proof]
