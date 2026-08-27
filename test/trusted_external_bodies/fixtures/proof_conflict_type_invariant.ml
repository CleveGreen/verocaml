let trusted () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.external_body]
[@@verocaml.type_invariant]
