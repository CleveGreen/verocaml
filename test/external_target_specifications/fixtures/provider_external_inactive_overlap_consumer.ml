[@@@verocaml.verify]

let unchanged value =
  let _ = Provider_external.anchor value in
  Provider_external_duplicate.anchor value
