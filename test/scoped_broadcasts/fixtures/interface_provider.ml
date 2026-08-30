type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type ('value, 'error) result_specification = ('value, 'error) result
[@@verocaml.external_type_specification]

type 'a box = { value : 'a }

[%%verocaml.symbolic val is_zero : int -> bool]
[%%verocaml.symbolic val is_nonnegative : int -> bool]
[%%verocaml.symbolic val option_present : 'a option -> bool]
[%%verocaml.symbolic val option_unwraps : 'a option -> bool]
[%%verocaml.symbolic
  val nested_present : (('a box option) option, 'error) result -> bool]
[%%verocaml.symbolic
  val nested_fact : (('a box option) option, 'error) result -> bool]
[%%verocaml.symbolic val reflexive_probe : 'a -> bool]

let zero_axiom (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((is_zero value) [@trigger])) || value = 0];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let nonnegative_axiom (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((is_nonnegative value) [@trigger])) || value >= 0];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let option_unwrap_axiom (value : 'a option) : unit =
  [%verocaml.ensures fun _ ->
    (not ((option_present value) [@trigger]))
    || option_unwraps value];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let nested_axiom
    (value : (('a box option) option, 'error) result) : unit =
  [%verocaml.ensures fun _ ->
    (not ((nested_present value) [@trigger])) || nested_fact value];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let reflexive_lemma (value : 'a) : unit =
  [%verocaml.ensures fun _ ->
    (not ((reflexive_probe value) [@trigger])) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

let ordinary_proof (value : int) : unit =
  [%verocaml.ensures fun _ -> value = value];
  ()
[@@verocaml.proof]

[@@@verocaml.broadcast_group (integer_facts, [zero_axiom; nonnegative_axiom])]
[@@@verocaml.broadcast_group (option_facts, [option_unwrap_axiom])]
[@@@verocaml.broadcast_group (nested_facts, [nested_axiom])]
[@@@verocaml.broadcast_group (proved_facts, [reflexive_lemma])]
