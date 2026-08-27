let countdown n =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result >= 0 && [%verocaml.old n] >= n];
  [%verocaml.decreases n - 1];
  [%verocaml.assert n >= 0];
  n
