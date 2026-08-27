let observe (value : int [@ghost]) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let bad source =
  let[@ghost] _observed = (observe source [@ghost]) in
  source
