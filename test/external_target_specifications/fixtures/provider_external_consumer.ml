[@@@verocaml.verify]

let promised value =
  [%verocaml.requires value < 100];
  [%verocaml.ensures fun result -> result = value + 1];
  let _ = Provider_external.anchor value in
  Legacy.promised value
