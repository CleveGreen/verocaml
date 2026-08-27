let value_exec_region (value : int) =
  let _proof_value =
    [%verocaml.proof
      [%verocaml.assert value = value];
      ()]
  in
  value
