let proof_region (value : int) =
  [%verocaml.proof [%verocaml.assert value = value]];
  value
