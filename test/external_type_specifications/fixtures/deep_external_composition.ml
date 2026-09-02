[@@@verocaml.verify]

type 'a option_catalog = 'a option
[@@verocaml.external_type_specification]

type 'a list_catalog = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_catalog = ('a, 'error) result
[@@verocaml.external_type_specification]

type 'a box_catalog = 'a External_types.box
[@@verocaml.external_type_specification]

type ('a, 'error) outcome_catalog = ('a, 'error) External_types.outcome
[@@verocaml.external_type_specification]

type 'a cell_catalog = 'a External_types.cell
[@@verocaml.external_type_specification]

[%%verocaml.symbolic
val observe_deep :
  ((((int option External_types.box, bool)
     External_types.outcome,
     int External_types.cell option)
    result)
   list) -> bool]

let lemma_deep_symbolic_reflexive
    (value :
      ((((int option External_types.box, bool)
         External_types.outcome,
         int External_types.cell option)
        result)
       list)) : unit =
  [%verocaml.ensures fun _ -> observe_deep value = observe_deep value];
  ()
[@@verocaml.proof]
