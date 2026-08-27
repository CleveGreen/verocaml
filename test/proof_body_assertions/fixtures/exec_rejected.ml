let ordinary_exec (x : int) =
  let y = x + 1 in
  [%verocaml.assert y > x]
