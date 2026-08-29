let proof_false (_condition : bool) : unit =
  assert false
[@@verocaml.proof]

let exec_false (_condition : bool) : unit =
  assert false

type 'a box = Box of 'a

let generic_false_sequence (_value : 'a box) : unit =
  assert false;
  ()
[@@verocaml.proof]
