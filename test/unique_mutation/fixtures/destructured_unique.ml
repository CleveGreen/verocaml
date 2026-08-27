type box = {
  mutable value : int;
  mutable flag : bool;
}

let increment_destructured
    ((box, delta) : box * int @ unique) : box @ unique =
  [%verocaml.requires
    delta = 1 && box.value < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result.value = [%verocaml.old box.value] + delta
    && result.flag = [%verocaml.old box.flag]];
  box.value <- box.value + delta;
  box
