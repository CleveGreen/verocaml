[@@@verocaml.verify]

let labelled_specification :
    first:int -> (second:int -> int) @ local =
 fun ~first ~second -> Curried_legacy.labelled ~first ~second
[@@verocaml.external_specification]
