let outer x =
  let inner y = y [@@verocaml.spec] in
  inner x
