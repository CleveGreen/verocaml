let consume_unit (_ : unit) = ()
[@@verocaml.proof]

let value_argument (x : int) =
  let y = x + 1 in
  consume_unit [%verocaml.assert y > x]
[@@verocaml.proof]
