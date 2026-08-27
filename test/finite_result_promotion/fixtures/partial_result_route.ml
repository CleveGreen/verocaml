type node = Empty | Node of int * node

let make value (xs : node [@finite]) = Node (value, xs)

let consume (_node : node [@finite]) : unit = ()

let use (xs : node [@finite]) =
  let partial = make 1 in
  consume (partial xs)
