let invalid x =
  [%verocaml.requires x >= 0];
  let y = x + 1 in
  [%verocaml.ensures fun result -> result = y];
  y
