type box = { mutable value : int }

let reject_capture (cell : box @ aliased) : unit =
  let captured () = cell.value in
  cell.value <- captured ()
