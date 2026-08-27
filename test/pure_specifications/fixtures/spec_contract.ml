let bad (x : int) : int =
  [%verocaml.requires x > 0];
  x + 1
[@@verocaml.spec]
