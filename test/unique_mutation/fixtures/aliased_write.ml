type box = { mutable value : int; mutable other : int }
let rejected (box : box @ aliased) =
  box.value <- 1
