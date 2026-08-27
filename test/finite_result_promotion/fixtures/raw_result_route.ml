type node = Empty | Node of int * node

let raw_make value = Node (value, Empty)
[@@verocaml.external_body]

let consume (_node : node [@finite]) : unit = ()

let use () = consume (raw_make 0)
