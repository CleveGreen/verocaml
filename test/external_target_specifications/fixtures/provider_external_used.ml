[@@@verocaml.verify]

let promised_specification (value : int) =
  [%verocaml.requires value < 100];
  [%verocaml.ensures fun result -> result = value + 1];
  Legacy.promised value
[@@verocaml.external_specification]

let promised value =
  [%verocaml.requires value < 100];
  [%verocaml.ensures fun result -> result = value + 1];
  Legacy.promised value
