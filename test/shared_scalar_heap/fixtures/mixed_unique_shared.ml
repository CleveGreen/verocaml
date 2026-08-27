type shared_box = { mutable shared_value : int }
type unique_box = { mutable unique_value : int }

let update_shared (cell : shared_box @ aliased) : unit =
  [%verocaml.ensures fun _ -> cell.shared_value = 7];
  cell.shared_value <- 7

let update_unique (cell : unique_box @ unique) : unique_box @ unique =
  [%verocaml.ensures fun result -> result.unique_value = 9];
  cell.unique_value <- 9;
  cell
