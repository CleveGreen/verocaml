let quantified_application (value : int) : int =
  [%verocaml.requires
    forall (fun (f : int -> int) -> (f value [@trigger]) = f value)];
  [%verocaml.requires exists (fun (f : int -> int) -> f value = f value)];
  [%verocaml.ensures fun result -> result = value];
  value
