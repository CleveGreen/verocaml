let trusted x =
  [%verocaml.ensures fun result -> result = x];
  x + true
[@@verocaml.external_body]
