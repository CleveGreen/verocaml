let proof_false (_condition : bool) : unit =
  assert false
[@@verocaml.proof]

let exec_false (_condition : bool) : unit =
  assert false
