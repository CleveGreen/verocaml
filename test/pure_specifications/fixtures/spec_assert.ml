let bad (x : int) : int =
  [%verocaml.assert x = x];
  x
[@@verocaml.spec]
