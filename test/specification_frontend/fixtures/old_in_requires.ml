let invalid x =
  [%verocaml.requires [%verocaml.old x] >= 0];
  x
