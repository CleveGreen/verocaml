let target (x : int) = x
let wrapper (x : int) =
  [%verocaml.assert x = x];
  target x
[@@verocaml.external_specification]
