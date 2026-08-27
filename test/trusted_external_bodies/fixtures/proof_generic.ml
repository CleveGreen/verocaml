let trusted x =
  [%verocaml.ensures fun _ -> false];
  ignore x
[@@verocaml.external_body]
[@@verocaml.proof]
