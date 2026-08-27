type box = { mutable value : int }

let reject_shadowing (cell : box @ aliased) : unit =
  let alias = cell in
  let cell = alias in
  cell.value <- 1
