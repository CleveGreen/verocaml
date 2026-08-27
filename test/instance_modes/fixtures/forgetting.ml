let spec_observe (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.spec]

let proof_observe (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let run source =
  let[@tracked] tracked = (source [@tracked]) in
  let[@ghost] ghost = (source [@ghost]) in
  [%verocaml.proof
    spec_observe source;
    spec_observe tracked;
    spec_observe ghost;
    proof_observe source;
    proof_observe tracked;
    proof_observe ghost];
  source
