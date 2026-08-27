let target (x : int @ unique) = x
let wrapper (x : int @ unique) = target x
[@@verocaml.external_specification]
