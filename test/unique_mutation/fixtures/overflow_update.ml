type box = { mutable value : int }
let overflow (box : box @ unique) : box @ unique =
  box.value <- box.value + 1;
  box
