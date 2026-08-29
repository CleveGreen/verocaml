type 'a forged = 'a Option.t
[@@verocaml.internal.external_type_specification.v1]

let identity (value : int Option.t) = value
