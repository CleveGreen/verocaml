let assertion value =
  [%verocaml.assert value = value];
  value
[@@verocaml.proof]
