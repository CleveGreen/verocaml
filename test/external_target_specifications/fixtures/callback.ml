[@@@verocaml.verify]
let promised_specification (value : int) = Legacy.promised value
[@@verocaml.external_specification]
let apply callback value = callback value
let bad value = apply Legacy.promised value
