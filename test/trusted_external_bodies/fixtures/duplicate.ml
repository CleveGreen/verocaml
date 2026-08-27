let trusted x =
  [%verocaml.ensures fun result -> result = x]; x
[@@verocaml.external_body] [@@verocaml.external_body]
