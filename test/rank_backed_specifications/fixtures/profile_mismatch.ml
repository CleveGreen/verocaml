type 'a node = Empty | Node of 'a * 'a node
type stack = { top : string node }

let identity (stack : stack) : stack = stack
[@@verocaml.spec]
