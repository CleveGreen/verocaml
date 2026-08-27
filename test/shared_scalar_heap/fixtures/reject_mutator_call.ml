type box = { mutable value : int }

let mutate (cell : box @ aliased) : unit =
  [%verocaml.ensures fun _ -> cell.value = 1];
  cell.value <- 1

let reject_mutator_call (cell : box @ aliased) : unit =
  mutate cell
