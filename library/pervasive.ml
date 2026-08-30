type 'a option_specification = 'a Option.t
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) Result.t
[@@verocaml.external_type_specification]
