type rank2 = { run : 'a. 'a -> 'a }

let use (value : int) : int =
  [%verocaml.assert
    let stored = { run = (fun argument -> argument) } in
    stored.run value = value];
  value
