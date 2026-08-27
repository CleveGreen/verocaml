let trusted (value : int [@ghost]) : (int [@ghost]) =
  [%verocaml.requires value >= 0];
  [%verocaml.ensures fun result -> result = value];
  (value [@ghost])
[@@verocaml.external_body]
