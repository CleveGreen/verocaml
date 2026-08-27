let bad (x : int) : int =
  [%verocaml.decreases x];
  x
[@@verocaml.spec]
