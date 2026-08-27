type node = Empty | Node of int * node

let rec left = Node (0, right)
and right = Node (1, left)

let make () = left

let consume (_node : node [@finite]) : unit = ()

let use () = consume (make ())
