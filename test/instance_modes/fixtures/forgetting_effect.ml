let runtime value = value + 1

let observe (value : int) : unit =
  let _ = value in
  ()
[@@verocaml.spec]

let bad source =
  [%verocaml.proof observe (runtime source)];
  source
