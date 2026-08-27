type 'a node =
  | Empty
  | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let consume_finite (node : int node [@finite]) = ()
[@@verocaml.proof]

let tracked_is_not_finite (stack : stack [@tracked]) =
  consume_finite stack.top
[@@verocaml.proof]

let bad stack =
  let[@tracked] tracked = (stack [@tracked]) in
  [%verocaml.proof tracked_is_not_finite (tracked [@tracked])];
  stack
