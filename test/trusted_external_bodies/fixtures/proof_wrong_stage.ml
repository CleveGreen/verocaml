let trusted () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.external_body]
[@@verocaml.proof]

let caller () = trusted ()
