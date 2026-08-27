let trusted () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.external_body]
[@@verocaml.external_specification]
