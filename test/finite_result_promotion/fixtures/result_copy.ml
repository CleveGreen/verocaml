type node = Empty | Node of int * node
type holder = { top : node }

let make value (xs : node [@finite]) : holder =
  { top = Node (value, xs) }

let consume (_holder : holder [@finite]) : unit = ()

let use (xs : node [@finite]) =
  let result = make 1 xs in
  let copied = result in
  consume copied
