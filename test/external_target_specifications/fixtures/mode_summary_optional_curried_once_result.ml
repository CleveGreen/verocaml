[@@@verocaml.verify]

let optional_specification :
    first:int -> (?second:int -> unit -> int) @ once =
 fun ~first ?second () -> Curried_legacy.optional ~first ?second ()
[@@verocaml.external_specification]
type 'a option_specification = 'a option
[@@verocaml.external_type_specification]
