type box = {
  mutable value : int;
  mutable flag : bool;
}

let increment_destructured
    ((box, delta) : box * int @ unique) : box @ unique =
  box.value <- box.value + delta;
  box
