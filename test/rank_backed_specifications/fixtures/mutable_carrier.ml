type 'a node = Empty | Node of 'a * 'a node
type stack = { mutable top : int node }

let identity (stack : stack) : stack = stack
[@@verocaml.spec]
