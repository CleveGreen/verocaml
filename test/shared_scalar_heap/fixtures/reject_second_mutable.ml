type box = {
  mutable left : int;
  mutable right : int;
}

let reject_second_mutable (cell : box @ aliased) : unit =
  cell.left <- 1
