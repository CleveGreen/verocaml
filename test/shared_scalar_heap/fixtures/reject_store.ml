type box = { mutable value : int }

let reject_store (cell : box @ aliased) : unit =
  let stored = ref cell in
  cell.value <- (!stored).value
