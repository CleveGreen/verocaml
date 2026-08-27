let rec left value =
  [%verocaml.decreases value];
  if value = 0 then 0 else right (value - 1)
and right value =
  [%verocaml.decreases value];
  if value = 0 then 0 else left (value - 1)
[@@verocaml.spec]
[@@verocaml.opaque]
