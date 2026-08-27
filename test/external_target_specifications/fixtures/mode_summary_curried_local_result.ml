[@@@verocaml.verify]

let curried_specification : int -> (int -> int) @ local =
 fun first second -> Curried_legacy.curried first second
[@@verocaml.external_specification]
