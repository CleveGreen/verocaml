let implies premise conclusion = (not premise) || conclusion
[@@verocaml.spec]

let iff a b = a = b
[@@verocaml.spec]

let admit () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.axiom]

let assume (cond : bool) =
  [%verocaml.ensures fun _ -> cond];
  admit ()
[@@verocaml.proof]

let affirm (cond : bool) =
  [%verocaml.requires cond];
  ()
[@@verocaml.proof]

type 'a option_specification = 'a Option.t
[@@verocaml.external_type_specification]

type 'a list_specification = 'a list
[@@verocaml.external_type_specification]

type ('a, 'error) result_specification = ('a, 'error) Result.t
[@@verocaml.external_type_specification]
