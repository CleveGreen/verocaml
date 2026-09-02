type 'a option_specification = 'a Option.t
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) Result.t
[@@verocaml.external_type_specification]

let identity value = value
[@@verocaml.spec]

let axiom_identity value =
  [%verocaml.ensures fun _ -> (identity value [@trigger]) = value];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]
