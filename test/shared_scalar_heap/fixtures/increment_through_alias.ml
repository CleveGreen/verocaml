type box = { mutable value : int }

let increment_through_alias (box : box @ aliased) : unit =
  [%verocaml.requires box.value < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun _ ->
    box.value = [%verocaml.old box.value] + 1];
  let alias = box in
  alias.value <- alias.value + 1
