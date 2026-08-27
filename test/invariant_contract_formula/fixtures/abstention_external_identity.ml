let external_identity (value : int) =
  [%verocaml.ensures fun result -> result = value];
  value
[@@verocaml.external_body]
