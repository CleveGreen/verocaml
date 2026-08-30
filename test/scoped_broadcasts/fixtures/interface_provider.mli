type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type ('value, 'error) result_specification = ('value, 'error) result
[@@verocaml.external_type_specification]

type 'a box = { value : 'a }

val is_zero : int -> bool
val is_nonnegative : int -> bool
val option_present : 'a option -> bool
val option_unwraps : 'a option -> bool
val nested_present : (('a box option) option, 'error) result -> bool
val nested_fact : (('a box option) option, 'error) result -> bool
val reflexive_probe : 'a -> bool

val zero_axiom : int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val nonnegative_axiom : int -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val option_unwrap_axiom : 'a option -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val nested_axiom : (('a box option) option, 'error) result -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val reflexive_lemma : 'a -> unit
[@@verocaml.proof]
[@@verocaml.broadcast]

val ordinary_proof : int -> unit
[@@verocaml.proof]

[@@@verocaml.broadcast_group (integer_facts, [zero_axiom; nonnegative_axiom])]
[@@@verocaml.broadcast_group (option_facts, [option_unwrap_axiom])]
[@@@verocaml.broadcast_group (nested_facts, [nested_axiom])]
[@@@verocaml.broadcast_group (proved_facts, [reflexive_lemma])]
