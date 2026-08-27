[@@@verocaml.verify]

let preserve (value : int) =
  [%verocaml.assert value + 0 = value];
  value
