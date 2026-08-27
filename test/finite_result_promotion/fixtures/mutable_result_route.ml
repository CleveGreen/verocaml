type node = Empty | Node of int * node
type holder = { mutable top : node }

let make value (xs : node [@finite]) : holder =
  { top = Node (value, xs) }

let consume (_node : node [@finite]) : unit = ()

let use (xs : node [@finite]) =
  let result = make 1 xs in
  consume result.top
