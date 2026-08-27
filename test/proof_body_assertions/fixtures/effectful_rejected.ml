let effectful (x : int) =
  let y = x + 1 in
  [%verocaml.assert (print_endline "not proof-pure"; y > x)]
[@@verocaml.proof]
