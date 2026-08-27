type node = Empty | Node of int * node

let target value = Node (value, Empty)

let external_make value = target value
[@@verocaml.external_specification]

let consume (_node : node [@finite]) : unit = ()

let use () = consume (external_make 0)
