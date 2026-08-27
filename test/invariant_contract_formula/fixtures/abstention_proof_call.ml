let lemma (value : int) =
  [%verocaml.ensures fun _result -> value = value];
  ()
[@@verocaml.proof]

let proof_call (value : int) =
  [%verocaml.proof lemma value];
  value
