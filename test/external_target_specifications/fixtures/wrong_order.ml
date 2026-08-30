[@@@verocaml.verify]
let select_specification ?value ~first () =
  Legacy.select ~first ?value ()
[@@verocaml.external_specification]
type 'a option_specification = 'a option
[@@verocaml.external_type_specification]
