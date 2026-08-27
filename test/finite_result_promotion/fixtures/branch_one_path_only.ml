type node = Empty | Node of int * node

let make value (xs : node [@finite]) : node = Node (value, xs)

let consume (_xs : node [@finite]) : unit = ()

let use_if choose (xs : node [@finite]) =
  let produced = make 1 xs in
  let selected = if choose then produced else xs in
  consume selected

let use_match choose (xs : node [@finite]) =
  let produced = make 2 xs in
  let selected = match choose with true -> produced | false -> xs in
  consume selected
