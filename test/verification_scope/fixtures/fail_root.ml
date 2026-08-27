[@@@verocaml.verify]

let reject (value : int) =
  [%verocaml.assert false];
  value
