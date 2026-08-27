let non_boolean_exec_region (value : int) =
  [%verocaml.proof
    [%verocaml.assert value];
    ()];
  value
