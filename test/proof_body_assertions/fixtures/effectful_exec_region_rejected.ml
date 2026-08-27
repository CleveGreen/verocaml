let effectful_exec_region (value : int) =
  [%verocaml.proof
    print_endline "not proof-pure";
    ()];
  value
