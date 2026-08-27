type 'a box = {
  mutable value : int;
  payload : 'a;
}

let reject_generic (cell : int box @ aliased) : unit =
  cell.value <- 1
