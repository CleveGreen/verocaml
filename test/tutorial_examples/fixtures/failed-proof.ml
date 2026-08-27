let wrong_successor (value : int) =
  [%verocaml.ensures fun result -> result = value + 1];
  value
