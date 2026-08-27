type box = { mutable value : int }

let rec drain_step (box : box @ unique) (n : int) : box @ unique =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result ->
    result.value = [%verocaml.old box.value]];
  [%verocaml.decreases n];
  [%verocaml.assert n >= 0];
  if n = 0 then box else drain_step box (n - 1)
