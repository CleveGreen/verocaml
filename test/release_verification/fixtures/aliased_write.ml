type box = { mutable value : int; mutable other : int }
let aliased_write (box : box @ aliased) = box.value <- 1
