type node = Empty | Node of int * node

let make value (xs : node [@finite]) = Node (value, xs)

let consume (_node : node [@finite]) : unit = ()

let apply maker (xs : node [@finite]) = consume (maker xs)

let use (xs : node [@finite]) = apply (make 1) xs
