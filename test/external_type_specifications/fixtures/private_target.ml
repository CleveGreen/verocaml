type 'a private_specification = 'a External_types.private_box
[@@verocaml.external_type_specification]

let identity (value : int External_types.private_box) = value
