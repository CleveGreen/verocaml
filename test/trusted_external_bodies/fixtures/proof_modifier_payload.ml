let trusted () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.external_body true]
[@@verocaml.proof]
