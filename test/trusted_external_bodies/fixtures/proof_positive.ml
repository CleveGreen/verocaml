let admit () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.external_body]
[@@verocaml.proof]

let assume (cond : bool) =
  [%verocaml.ensures fun _ -> cond];
  admit ()
[@@verocaml.proof]

let caller (cond : bool) =
  [%verocaml.requires cond];
  [%verocaml.ensures fun _ -> cond];
  assume cond
[@@verocaml.proof]
