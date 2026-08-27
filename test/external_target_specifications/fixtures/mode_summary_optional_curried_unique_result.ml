[@@@verocaml.verify]

let optional_specification :
    first:int -> (?second:int -> unit -> int) @ unique =
 fun ~first ?second () -> Curried_legacy.optional ~first ?second ()
[@@verocaml.external_specification]
