let trusted x =
  [%verocaml.ensures fun result -> result >= x];
  [%verocaml.decreases x];
  x
[@@verocaml.external_body]
