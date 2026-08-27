[@@@verocaml.verify]
module L = Legacy
let promised_specification (value : int) = L.promised value
[@@verocaml.external_specification]
let bad (value : int) = L.promised value
