type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let bad (value : 'a option) = value = value
