let trusted x =
  [%verocaml.ensures fun result -> result >= x];
  [%verocaml.assert x >= 0];
  x
[@@verocaml.external_body]
