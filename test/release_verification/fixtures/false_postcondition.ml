let false_postcondition (x : int) =
  [%verocaml.ensures fun result -> result = x + 1];
  x
