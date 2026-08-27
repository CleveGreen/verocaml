type box = {
  mutable value : int;
  mutable flag : bool;
}

let increment (box : box @ unique) : box @ unique =
  [%verocaml.requires box.value < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result.value = [%verocaml.old box.value] + 1
    && result.flag = [%verocaml.old box.flag]];
  box.value <- box.value + 1;
  box

let increment_then_read (box : box @ unique) =
  [%verocaml.requires box.value < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result = [%verocaml.old box.value] + 1];
  let box = increment box in
  box.value

let local_mutable () =
  [%verocaml.ensures fun result -> result = 1];
  let mutable value = 0 in
  value <- value + 1;
  value
