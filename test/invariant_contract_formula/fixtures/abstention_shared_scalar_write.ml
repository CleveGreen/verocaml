type cell = { mutable value : int }
let shared_scalar_write (cell : cell @ aliased) =
  cell.value <- 0;
  ()
