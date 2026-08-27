type box = {
  mutable value : int;
  next : box;
}

let reject_recursive_record (cell : box @ aliased) : unit =
  cell.value <- 1
