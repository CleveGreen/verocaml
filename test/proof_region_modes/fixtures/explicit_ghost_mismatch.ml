let observe_explicit (value : int [@ghost]) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let bad source =
  [%verocaml.proof observe_explicit source];
  source
