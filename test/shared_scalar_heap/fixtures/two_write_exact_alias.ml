type box = { mutable value : int }

let two_write_exact_alias (cell : box @ aliased) : unit =
  [%verocaml.requires
    -2_305_843_009_213_693_950 <= cell.value
    && cell.value <= 2_305_843_009_213_693_949];
  [%verocaml.ensures fun _ ->
    cell.value =
      [%verocaml.old cell.value] + [%verocaml.old cell.value] + 2];
  let peer = cell in
  cell.value <- cell.value + peer.value;
  peer.value <- peer.value + 2
