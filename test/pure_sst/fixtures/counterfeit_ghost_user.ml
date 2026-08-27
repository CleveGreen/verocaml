let counterfeit_ghost (x : int) =
  [%verocaml.requires x >= 0];
  x + 1
