let local_true (x : int) =
  let y = x + 1 in
  [%verocaml.assert y > x]
[@@verocaml.proof]
