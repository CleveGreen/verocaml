let observe_tracked (value : int [@tracked]) : unit =
  let _ = (value [@tracked]) in
  ()
[@@verocaml.proof]

let bad source =
  [%verocaml.proof
    let[@ghost] ghost = (source [@ghost]) in
    observe_tracked (ghost [@tracked])];
  source
