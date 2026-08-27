[@@@verocaml.verify]
let select_specification ~(first : int) ?value () =
  Legacy.select ~first ?value ()
[@@verocaml.external_specification]
