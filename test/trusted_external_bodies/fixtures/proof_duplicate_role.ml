let trusted () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.proof]
[@@verocaml.external_body]
[@@verocaml.proof]
