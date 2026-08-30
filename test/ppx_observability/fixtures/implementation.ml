let increment value =
  [%verocaml.requires value >= 0];
  value + 1
