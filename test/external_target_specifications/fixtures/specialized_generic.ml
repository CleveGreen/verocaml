[@@@verocaml.verify]
let select_specification ~(first : int) ?value () =
  Legacy.select ~first ?value ()
[@@verocaml.external_specification]
type 'a option_specification = 'a option
[@@verocaml.external_type_specification]
