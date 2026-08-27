let non_unit (x : int) =
  let y = x + 1 in
  ([%verocaml.assert y > x] : int)
[@@verocaml.proof]
