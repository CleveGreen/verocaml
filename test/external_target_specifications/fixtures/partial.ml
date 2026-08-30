[@@@verocaml.verify]
let select_specification ~(first : 'a) ?value () = Legacy.select ~first ?value ()
[@@verocaml.external_specification]
let bad () = Legacy.select ~first:1
type 'a option_specification = 'a option
[@@verocaml.external_type_specification]
