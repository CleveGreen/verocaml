type box = { mutable value : int }

let remove_first_write (cell : box @ aliased) : unit =
  [%verocaml.ensures fun _ ->
    cell.value =
      [%verocaml.old cell.value] + [%verocaml.old cell.value] + 2];
  cell.value <- cell.value + 2
