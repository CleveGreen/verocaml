let first (flag : bool) =
  [%verocaml.ensures fun _ -> false];
  if flag then () else ()
[@@verocaml.external_body]
[@@verocaml.proof]

let second (flag : bool) =
  [%verocaml.ensures fun _ -> false];
  if flag then () else ()
[@@verocaml.proof]
[@@verocaml.external_body]
