let trusted () =
  [%verocaml.ensures fun _ -> false];
  1 + true
[@@verocaml.external_body]
[@@verocaml.proof]
