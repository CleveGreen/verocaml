type box = { mutable value : int }

let reject_return (cell : box @ aliased) : box =
  cell.value <- 1;
  cell
