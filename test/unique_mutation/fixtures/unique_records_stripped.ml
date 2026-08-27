type box = {
  mutable value : int;
  mutable flag : bool;
}

let increment (box : box @ unique) : box @ unique =
  box.value <- box.value + 1;
  box

let increment_then_read (box : box @ unique) =
  let box = increment box in
  box.value

let local_mutable () =
  let mutable value = 0 in
  value <- value + 1;
  value
