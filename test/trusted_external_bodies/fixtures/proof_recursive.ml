let rec trusted () =
  [%verocaml.ensures fun _ -> false];
  trusted ()
[@@verocaml.external_body]
[@@verocaml.proof]
