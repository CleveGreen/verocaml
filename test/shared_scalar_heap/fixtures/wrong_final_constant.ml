type box = { mutable value : int }

let wrong_final_constant (cell : box @ aliased) : unit =
  [%verocaml.requires
    -2_305_843_009_213_693_950 <= cell.value
    && cell.value <= 2_305_843_009_213_693_949];
  [%verocaml.ensures fun _ ->
    cell.value =
      [%verocaml.old cell.value] + [%verocaml.old cell.value] + 3];
  let peer = cell in
  cell.value <- cell.value + peer.value;
  peer.value <- peer.value + 2
