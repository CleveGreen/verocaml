[@@@verocaml.verify]
let promised_specification (value : int) = Legacy.promised value
[@@verocaml.external_specification]
let bad (value : int) =
  let alias = Legacy.promised in
  alias value
