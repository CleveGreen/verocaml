let escaping_local_cell () =
  let mutable cell = 0 in
  fun () ->
    cell <- cell + 1;
    cell
