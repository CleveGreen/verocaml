let target (x : int) = x
let wrapper (x : int) =
  [%verocaml.decreases x];
  target x
[@@verocaml.external_specification]
