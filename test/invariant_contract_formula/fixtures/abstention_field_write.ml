type cell = { mutable value : int }
let field_write (cell : cell @ unique) =
  cell.value <- 0;
  cell
