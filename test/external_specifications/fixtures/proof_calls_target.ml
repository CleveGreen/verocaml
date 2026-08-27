let target (x : int) = x
let wrapper (x : int) = target x
[@@verocaml.external_specification]
let lemma (x : int) : unit =
  let _ = target x in
  ()
[@@verocaml.proof]
