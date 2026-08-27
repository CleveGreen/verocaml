let invalid x =
  let y = x + 1 in
  [%verocaml.assert y > x];
  y
