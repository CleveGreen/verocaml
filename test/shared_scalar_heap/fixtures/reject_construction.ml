type box = { mutable value : int }

let reject_construction (cell : box @ aliased) : unit =
  let fresh = { value = cell.value } in
  cell.value <- fresh.value
