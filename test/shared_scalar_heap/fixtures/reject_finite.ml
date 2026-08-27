type box = { mutable value : int }

let reject_finite (cell : (box [@finite]) @ aliased) : unit =
  cell.value <- 1
