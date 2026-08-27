[@@@verocaml.verify]
let promised_specification (value : int [@ghost]) =
  let _ = value in
  Mode_legacy.promised
[@@verocaml.external_specification]
