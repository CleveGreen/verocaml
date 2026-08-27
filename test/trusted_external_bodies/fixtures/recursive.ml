let rec trusted x =
  [%verocaml.ensures fun result -> result = x];
  trusted x
[@@verocaml.external_body]
