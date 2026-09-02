type 'a option_specification = 'a Option.t
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) Result.t
[@@verocaml.external_type_specification]

val identity : 'a -> 'a
[@@verocaml.spec]

val axiom_identity : 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]
