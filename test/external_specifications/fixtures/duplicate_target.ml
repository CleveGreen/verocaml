let target (x : int) = x
let wrapper1 (x : int) = target x
[@@verocaml.external_specification]
let wrapper2 (x : int) = target x
[@@verocaml.external_specification]
