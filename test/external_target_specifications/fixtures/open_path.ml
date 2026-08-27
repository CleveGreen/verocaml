[@@@verocaml.verify]
open Legacy
let promised_specification (value : int) = promised value
[@@verocaml.external_specification]
let bad (value : int) = promised value
