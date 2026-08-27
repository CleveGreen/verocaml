type node = Empty | Node of int * node

let identity (node : node) : node = node
[@@verocaml.spec]
