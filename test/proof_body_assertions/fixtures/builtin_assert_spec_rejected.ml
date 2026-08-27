let spec_rejected (condition : bool) : unit =
  assert condition
[@@verocaml.spec]
