[@@@verocaml.verify]

let unchanged value =
  [%verocaml.ensures fun result -> result = value];
  Provider_external.anchor value
