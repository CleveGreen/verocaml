[@@@verocaml.verify]

let promised_specification (value : int) : int @ unique =
  Legacy.promised value
[@@verocaml.external_specification]
