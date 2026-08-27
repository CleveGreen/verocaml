let unchecked f x =
  [%verocaml.requires true];
  [%verocaml.ensures fun _ -> true];
  f x
