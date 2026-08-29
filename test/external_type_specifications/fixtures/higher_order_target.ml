type 'a higher_order_specification = 'a External_types.higher_order
[@@verocaml.external_type_specification]

let identity (value : int External_types.higher_order) = value
