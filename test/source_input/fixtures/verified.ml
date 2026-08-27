let identity (x : int) : int =
  [%verocaml.ensures fun result -> result = x];
  x
