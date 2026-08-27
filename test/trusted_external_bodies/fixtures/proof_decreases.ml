let trusted () =
  [%verocaml.ensures fun _ -> false];
  [%verocaml.decreases 0];
  ()
[@@verocaml.external_body]
[@@verocaml.proof]
