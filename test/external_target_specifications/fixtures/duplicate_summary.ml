[@@@verocaml.verify]
let first (value : int) = Legacy.promised value
[@@verocaml.external_specification]
let second (value : int) = Legacy.promised value
[@@verocaml.external_specification]
