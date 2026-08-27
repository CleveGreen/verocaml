type box = { mutable value : int }

let reject_default_mode (cell : box) : unit =
  cell.value <- 1
