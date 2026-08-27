type box = { mutable value : int }

let wrong_mutation (box : box @ unique) : box @ unique =
  [%verocaml.requires box.value < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result.value = [%verocaml.old box.value] + 2];
  box.value <- box.value + 1;
  box
