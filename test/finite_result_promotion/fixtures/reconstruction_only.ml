type node = Empty | Node of int * node
type holder = { top : node }

let make value (xs : node [@finite]) : node = Node (value, xs)

let constructor_rebuild (xs : node [@finite]) : node =
  let made = make 1 xs in
  Node (2, made)

let record_rebuild (xs : node [@finite]) : holder =
  let made = make 1 xs in
  { top = made }
