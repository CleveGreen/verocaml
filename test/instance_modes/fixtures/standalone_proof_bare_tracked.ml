let nonnegative value = value >= 0 [@@verocaml.spec]

let observe (value : int [@tracked]) =
  [%verocaml.assert value = value];
  let[@tracked] local = (value [@tracked]) in
  [%verocaml.assert local = value];
  let _ = nonnegative local in
  ()
[@@verocaml.proof]
