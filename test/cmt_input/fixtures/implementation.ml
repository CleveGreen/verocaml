type box = { mutable value : int }

let update (box : box @ unique) =
  let before = box.value in
  box.value <- before + 1;
  box

let local_cell n =
  let mutable x = n in
  x <- x + 1;
  x

let refs n =
  let r = ref n in
  r := !r + 1;
  !r

let direct n = update { value = n }
