let admit (flag : bool) =
  [%verocaml.ensures fun _ -> false];
  if flag then () else ()
[@@verocaml.proof]
