type box = { mutable value : int }

let reject_third_write (cell : box @ aliased) : unit =
  cell.value <- 1;
  cell.value <- 2;
  cell.value <- 3
