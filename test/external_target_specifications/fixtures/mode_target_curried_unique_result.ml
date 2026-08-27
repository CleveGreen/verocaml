[@@@verocaml.verify]

let curried_specification : int -> int -> int =
 fun first second -> Mode_curried_legacy.curried first second
[@@verocaml.external_specification]
