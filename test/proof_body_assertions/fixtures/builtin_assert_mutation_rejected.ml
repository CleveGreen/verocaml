type cell = { mutable value : int }

let mutation_rejected (cell : cell) : unit =
  assert (cell.value <- cell.value + 1; true)
