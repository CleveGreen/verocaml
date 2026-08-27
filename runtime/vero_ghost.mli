val requires : (unit -> bool) -> unit
val ensures : ('a -> bool) -> unit
val decreases : (unit -> 'a) -> unit
val assert_ : (unit -> bool) -> unit
val old : 'a -> 'a
val marker : string -> unit
val sidecar : string -> unit
val spec_definition : string -> (unit -> 'a) -> 'a
val recursive_spec_definition : string -> string -> (unit -> 'a) -> 'a
val proof_definition : string -> (unit -> 'a) -> 'a
val proof_region : string -> (unit -> unit) -> unit
val type_invariant_definition : string -> (unit -> 'a) -> 'a
val use_type_invariant : string -> 'a @ read -> unit
val reveal : string -> 'a -> unit
val reveal_with_fuel : string -> string -> 'a -> unit
val external_specification : string -> (unit -> 'a) -> 'a
val external_body : string -> (unit -> 'a @ unique) @ once -> 'a @ unique
