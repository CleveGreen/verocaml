[@@@verocaml.verify]
let select_specification ~first:(other : 'b) ?value:chosen () =
  Legacy.select ~first:other ?value:chosen ()
[@@verocaml.external_specification]
let selected value = Legacy.select ~first:value ()
