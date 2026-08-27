let target (x : int) =
  [%verocaml.assert false];
  [%verocaml.ensures fun _ -> false];
  [%verocaml.decreases x];
  x * x
let wrapper (x : int) = target x
[@@verocaml.external_specification]
let caller (x : int) = target x
