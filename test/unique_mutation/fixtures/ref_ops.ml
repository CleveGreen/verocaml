let rejected () =
  let cell = ref 0 in
  cell := !cell + 1;
  !cell
