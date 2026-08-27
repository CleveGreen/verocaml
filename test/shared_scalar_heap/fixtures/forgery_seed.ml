type box = { mutable value : int }

let forgery_seed (cell : box @ aliased) : unit =
  cell.value <- 1
