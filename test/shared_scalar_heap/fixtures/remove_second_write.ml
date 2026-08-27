type box = { mutable value : int }

let remove_second_write (cell : box @ aliased) : unit =
  [%verocaml.ensures fun _ ->
    cell.value =
      [%verocaml.old cell.value] + [%verocaml.old cell.value] + 2];
  let peer = cell in
  cell.value <- cell.value + peer.value
