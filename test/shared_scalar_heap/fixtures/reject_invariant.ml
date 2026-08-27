type box = { mutable value : int }

let invariant (cell : box @ read) = cell.value >= 0
[@@verocaml.type_invariant]

let reject_invariant (cell : box @ aliased) : unit =
  cell.value <- 1
