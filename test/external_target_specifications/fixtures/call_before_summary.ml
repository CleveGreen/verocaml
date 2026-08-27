[@@@verocaml.verify]
let bad (value : int) = Legacy.promised value
let promised_specification (value : int) = Legacy.promised value
[@@verocaml.external_specification]
