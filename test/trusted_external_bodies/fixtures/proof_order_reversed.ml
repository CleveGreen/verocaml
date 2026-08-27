let admit () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.proof]
[@@verocaml.external_body]

let assume (cond : bool) =
  [%verocaml.ensures fun _ -> cond];
  admit ()
[@@verocaml.proof]
