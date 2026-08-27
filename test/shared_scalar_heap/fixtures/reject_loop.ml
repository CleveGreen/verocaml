type box = { mutable value : int }

let reject_loop (cell : box @ aliased) : unit =
  while cell.value < 1 do
    cell.value <- cell.value + 1
  done
