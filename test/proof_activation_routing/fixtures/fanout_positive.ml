type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let rec spec_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let rec seeded_node_len (node : int node) : int =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + seeded_node_len next
[@@verocaml.spec] [@@verocaml.revealed]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let stack_wf (stack : stack) : bool =
  spec_node_len stack.top = stack.length
[@@verocaml.spec]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let lemma_empty_stack (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result -> stack_wf stack];
  [%verocaml.reveal_with_fuel (spec_node_len, 2)]
[@@verocaml.proof]

let lemma_empty_stack_with_call (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result -> stack_wf stack];
  [%verocaml.reveal_with_fuel (spec_node_len, 2)];
  affirm (spec_node_len Empty = 0)
[@@verocaml.proof]

let one_path_control (_dummy : bool) =
  [%verocaml.reveal_with_fuel (spec_node_len, 2)];
  affirm (spec_node_len Empty = 0)
[@@verocaml.proof]

let multiple_postconditions (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result -> stack_wf stack];
  [%verocaml.ensures fun _result ->
    spec_node_len stack.top = stack.length
    && spec_node_len Empty = 0];
  [%verocaml.reveal_with_fuel (spec_node_len, 2)]
[@@verocaml.proof]

let seeded_fanout (stack : stack [@finite]) =
  [%verocaml.requires
    stack.length = 0 && spec_is_empty stack.top];
  [%verocaml.ensures fun _result ->
    seeded_node_len stack.top = stack.length];
  ()
[@@verocaml.proof]

let nested_branch_match (take_left : bool) (node : int node [@finite]) =
  if take_left then
    (match node with
    | Empty ->
        [%verocaml.reveal_with_fuel (spec_node_len, 2)];
        affirm (spec_node_len Empty = 0)
    | Node _ ->
        [%verocaml.reveal_with_fuel (spec_node_len, 2)])
  else
    [%verocaml.reveal_with_fuel (spec_node_len, 1)]
[@@verocaml.proof]
