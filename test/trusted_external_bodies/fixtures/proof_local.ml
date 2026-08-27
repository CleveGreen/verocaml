let outer () =
  let trusted () =
    [%verocaml.ensures fun _ -> false];
    ()
  [@@verocaml.external_body]
  [@@verocaml.proof]
  in
  trusted ()
