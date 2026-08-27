type cell = { mutable value : int }
let transition_operation (cell : cell @ unique) =
  cell.value <- cell.value + 1;
  cell
