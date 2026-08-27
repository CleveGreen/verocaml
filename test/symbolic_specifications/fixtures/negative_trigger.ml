[%%verocaml.symbolic val constant : bool]

let nullary_trigger (value : int) : int =
  [%verocaml.requires
    forall (fun (_candidate : int) -> (constant [@trigger]))];
  [%verocaml.ensures fun result -> result = value];
  value
