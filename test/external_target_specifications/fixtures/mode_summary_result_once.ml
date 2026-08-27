[@@@verocaml.verify]

let promised_specification (value : int) : int @ once =
  Legacy.promised value
[@@verocaml.external_specification]
