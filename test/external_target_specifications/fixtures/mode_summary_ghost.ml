[@@@verocaml.verify]
let promised_specification (value : int [@ghost]) =
  Legacy.promised (value [@ghost])
[@@verocaml.external_specification]
