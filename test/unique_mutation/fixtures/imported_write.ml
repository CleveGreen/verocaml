let rejected (box : Imported_box.t @ unique) : Imported_box.t @ unique =
  box.value <- 1;
  box
