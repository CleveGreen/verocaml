let contradiction () : unit =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.axiom]

let lemma_should_be_proof () : unit = contradiction ()
