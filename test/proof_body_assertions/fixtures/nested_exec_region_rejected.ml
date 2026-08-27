let nested_exec_region (value : int) =
  [%verocaml.proof
    ([%verocaml.proof ()];
     ());
    ()];
  value
