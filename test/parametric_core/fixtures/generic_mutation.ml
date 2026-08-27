let bad value =
  let mutable current = value in
  current <- value;
  current
