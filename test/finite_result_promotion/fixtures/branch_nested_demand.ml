type node = Empty | Node of int * node

let make value (xs : node [@finite]) : node = Node (value, xs)

let consume (_xs : node [@finite]) : unit = ()

let use choose (xs : node [@finite]) =
  if choose then (
    let produced = make 1 xs in
    consume produced)
  else ()
