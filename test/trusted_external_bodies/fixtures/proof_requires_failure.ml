let trusted (cond : bool) =
  [%verocaml.requires cond];
  [%verocaml.ensures fun _ -> cond];
  ()
[@@verocaml.external_body]
[@@verocaml.proof]

let caller (_flag : bool) = trusted false
[@@verocaml.proof]
