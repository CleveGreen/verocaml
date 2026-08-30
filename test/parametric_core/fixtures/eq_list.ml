type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

let bad (value : 'a list) = value <> value
