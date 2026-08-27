let rejected (f : int -> int -> int) x =
  [%verocaml.requires true];
  [%verocaml.ensures fun _ -> true];
  f x
