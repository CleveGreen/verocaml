[@@@verocaml.verify]
type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let select_specification ~first:(other : 'b) ?value:chosen () =
  Legacy.select ~first:other ?value:chosen ()
[@@verocaml.external_specification]
let selected value = Legacy.select ~first:value ()
