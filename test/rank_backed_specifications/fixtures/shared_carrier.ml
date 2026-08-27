type 'a node = Empty | Node of 'a * 'a node
type stack = { top : int node @@ aliased }

let identity (stack : stack) : stack = stack
[@@verocaml.spec]
