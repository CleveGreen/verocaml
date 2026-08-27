let specification_context (x : int) =
  let y = x + 1 in
  [%verocaml.assert y > x]
[@@verocaml.spec]
