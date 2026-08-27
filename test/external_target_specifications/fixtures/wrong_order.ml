[@@@verocaml.verify]
let select_specification ?value ~first () =
  Legacy.select ~first ?value ()
[@@verocaml.external_specification]
