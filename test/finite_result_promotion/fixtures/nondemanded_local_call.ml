type node = Empty | Node of int * node

let make value (xs : node [@finite]) : node = Node (value, xs)

let run (xs : node [@finite]) =
  let _unused = make 1 xs in
  0
