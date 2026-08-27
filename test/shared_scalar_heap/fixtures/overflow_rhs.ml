type box = { mutable value : int }

let overflow_rhs (cell : box @ aliased) : unit =
  cell.value <- cell.value + 1
