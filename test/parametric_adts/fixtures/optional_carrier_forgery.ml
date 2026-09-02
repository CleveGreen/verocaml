[@@@verocaml.verify]

type 'a lookalike_specification = 'a Optional_carrier_shapes.lookalike
[@@verocaml.external_type_specification]

let forged_optional ?(value = 0) () = value
