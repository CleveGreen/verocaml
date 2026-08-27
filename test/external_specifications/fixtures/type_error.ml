let target (x : int) = x
let wrapper (x : bool) : int = target x
[@@verocaml.external_specification]
