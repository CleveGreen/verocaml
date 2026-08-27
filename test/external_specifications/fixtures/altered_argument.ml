let target (x : int) = x
let wrapper (x : int) = target (x + 1)
[@@verocaml.external_specification]
