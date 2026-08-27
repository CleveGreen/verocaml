let explicit_exec_region (x : int) =
  [%verocaml.proof
    let y = x + 1 in
    [%verocaml.assert y > x]]
