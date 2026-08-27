type box = { mutable value : int }

let scalar_identity (value : int) = value

let reject_call (cell : box @ aliased) : unit =
  cell.value <- scalar_identity cell.value
