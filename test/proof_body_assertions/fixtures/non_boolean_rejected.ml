let non_boolean (x : int) =
  let y = x + 1 in
  [%verocaml.assert y]
[@@verocaml.proof]
