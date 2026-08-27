[@@@verocaml.verify]

let identity (value : int) =
  [%verocaml.assert value = value];
  value
