type 'a node = Empty | Node of 'a * 'a node
type stack = { top : int node; length : int }

let rec spec_push_n_bad
    (value : int)
    (n : int)
    (stack : stack) : stack =
  [%verocaml.decreases n];
  if n <= 0 then stack
  else spec_push_n_bad value n
      { top = Node (value, stack.top); length = stack.length + 1 }
[@@verocaml.spec]
[@@verocaml.opaque]
