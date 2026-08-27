let trusted () =
  [%verocaml.ensures fun result -> result = 1];
  1
[@@verocaml.external_body]
[@@verocaml.proof]
