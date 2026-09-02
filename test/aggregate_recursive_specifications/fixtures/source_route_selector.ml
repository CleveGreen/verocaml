type node = Empty | Node of int * node

type stack = {
  top : node;
  length : Vstd.Int.t;
}

let rec spec_repeat (value : int) (n : Vstd.Int.t) (stack : stack) : stack =
  [%verocaml.decreases n];
  if n <= 0 then stack
  else
    spec_repeat value (n - 1)
      { top = Node (value, stack.top); length = stack.length + 1 }
[@@verocaml.spec]
[@@verocaml.opaque]

let selector_route (value : int) (n : int) (stack : stack [@finite]) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result ->
  let expected = spec_repeat value n stack in
    expected.length = expected.length];
  ()
[@@verocaml.proof]
