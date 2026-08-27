[@@@verocaml.verify]

let promised_specification (value : int @ local) =
  Legacy.promised value
[@@verocaml.external_specification]
