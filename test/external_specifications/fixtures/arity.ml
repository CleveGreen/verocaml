let target (x : int) (y : int) = x + y
let wrapper (x : int) (y : int) = target x x
[@@verocaml.external_specification]
