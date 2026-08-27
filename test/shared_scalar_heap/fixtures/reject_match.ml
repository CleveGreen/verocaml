type box = { mutable value : int }

let reject_match (cell : box @ aliased) (flag : bool) : unit =
  match flag with
  | true -> cell.value <- 1
  | false -> cell.value <- 2
