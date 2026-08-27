let observe (xs : int Import_provider.seq) : bool =
  Import_provider.same_shape xs
[@@verocaml.spec]
