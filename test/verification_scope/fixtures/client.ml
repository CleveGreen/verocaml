[@@@verocaml.verify]

let call (value : int) =
  [%verocaml.assert value = value];
  Provider.identity value
