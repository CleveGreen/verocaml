let bad (value : int) : unit =
  [%verocaml.use_type_invariant value]
[@@verocaml.proof]
