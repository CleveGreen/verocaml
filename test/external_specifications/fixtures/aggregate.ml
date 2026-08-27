let target ((x, y) : int * int) = x + y
let wrapper ((x, y) : int * int) = target (x, y)
[@@verocaml.external_specification]
