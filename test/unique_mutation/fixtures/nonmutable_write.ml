type box = { value : int }
let rejected (box : box @ unique) =
  box.value <- 1
