type node = Empty | Node of int * node

let rec cycle = Node (0, cycle)

let make () = cycle

let consume (_node : node [@finite]) : unit = ()

let use () = consume (make ())
