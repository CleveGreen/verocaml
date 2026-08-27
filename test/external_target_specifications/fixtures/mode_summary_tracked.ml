[@@@verocaml.verify]
let promised_specification (value : int [@tracked]) =
  Legacy.promised (value [@tracked])
[@@verocaml.external_specification]
