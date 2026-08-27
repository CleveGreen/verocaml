type node = Empty | Node of int * node
type holder = { top : node }
type wider = { payload : holder; marker : int }

let make value (xs : node [@finite]) : holder =
  { top = Node (value, xs) }

let consume (_wider : wider [@finite]) : unit = ()

let use (xs : node [@finite]) =
  let result = make 1 xs in
  let widened = { payload = result; marker = 0 } in
  consume widened
