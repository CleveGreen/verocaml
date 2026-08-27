let trusted () =
  [%verocaml.ensures fun _ -> false];
  [%verocaml.assert false];
  ()
[@@verocaml.external_body]
[@@verocaml.proof]
