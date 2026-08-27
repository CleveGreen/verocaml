[@@@verocaml.verify]

let promised_specification (value : int) : int @ local =
  Legacy.promised value
[@@verocaml.external_specification]

let use value = Legacy.promised value
