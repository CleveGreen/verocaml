[@@@verocaml.verify]

let curried_specification first second = Curried_legacy.curried first second
[@@verocaml.external_specification]

let combined first second = Curried_legacy.curried first second
