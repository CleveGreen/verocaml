[@@@verocaml.verify]

let promised value =
  [%verocaml.requires value < 100];
  let _ = Provider_external.anchor value in
  let _ = Provider_external_duplicate.anchor value in
  Legacy.promised value
