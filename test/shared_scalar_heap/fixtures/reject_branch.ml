type box = { mutable value : int }

let reject_branch (cell : box @ aliased) (flag : bool) : unit =
  if flag then cell.value <- 1 else cell.value <- 2
