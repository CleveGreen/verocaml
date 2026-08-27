type box = { mutable value : int }

let old_one_write (cell : box @ aliased) : unit =
  [%verocaml.requires cell.value = 10];
  [%verocaml.ensures fun _ ->
    [%verocaml.old cell.value] = 10 && cell.value = 11];
  cell.value <- 11

let old_two_writes (cell : box @ aliased) : unit =
  [%verocaml.requires cell.value = 10];
  [%verocaml.ensures fun _ ->
    [%verocaml.old cell.value] = 10 && cell.value = 12];
  cell.value <- 11;
  cell.value <- 12
