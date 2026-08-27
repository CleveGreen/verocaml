let rec loop value =
  [%verocaml.requires value >= 0];
  [%verocaml.decreases value];
  if value = 0 then () else loop (value - 1)
