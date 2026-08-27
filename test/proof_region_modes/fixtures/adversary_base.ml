let observe (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.proof]

let attack aa bb =
  [%verocaml.proof
    observe aa;
    observe bb];
  aa + bb
