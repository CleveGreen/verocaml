let rec decreases_to_zero (i : int) : int =
  [%verocaml.requires i >= 0];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases i];
  if i = 0 then i
  else decreases_to_zero (i - 1)
