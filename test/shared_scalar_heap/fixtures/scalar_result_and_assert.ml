type box = { mutable value : int }

let scalar_result_and_assert (cell : box @ aliased) : int =
  [%verocaml.ensures fun result -> result = 3 && cell.value = 3];
  [%verocaml.assert cell.value = cell.value];
  cell.value <- 3;
  cell.value
