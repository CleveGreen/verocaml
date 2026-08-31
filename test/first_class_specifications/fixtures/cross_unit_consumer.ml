let use (value : int) : int =
  [%verocaml.assert Negative_cross_unit.increment value = value + 1];
  value
