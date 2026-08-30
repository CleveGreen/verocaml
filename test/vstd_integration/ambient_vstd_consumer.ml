let identity value =
  [%verocaml.ensures fun result -> result = value];
  value
