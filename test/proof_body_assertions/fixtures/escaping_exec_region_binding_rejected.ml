let escaping_exec_region_binding (value : int) =
  [%verocaml.proof
    let proof_only = value in
    let _ = proof_only in
    ()];
  proof_only
