[@@@verocaml.verify]

type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) result
[@@verocaml.external_type_specification]

type 'a box_specification = 'a Foreign_external_types.box
[@@verocaml.external_type_specification]

type ('a, 'error) outcome_specification =
  ('a, 'error) Foreign_external_types.outcome
[@@verocaml.external_type_specification]

[%%verocaml.symbolic
val observes_box : 'a Foreign_external_types.box -> bool]

let lemma_observes_box_reflexive
    (value : 'a Foreign_external_types.box) : unit =
  [%verocaml.ensures fun _ -> observes_box value = observes_box value];
  ()
[@@verocaml.proof]

let make_nested_option (value : int) : int option option =
  Some (Some value)

let make_deep_value (value : int) :
      ((int option Foreign_external_types.box, bool)
       Foreign_external_types.outcome,
       int option)
      result =
  Ok (Foreign_external_types.Good (Foreign_external_types.Box (Some value)))
