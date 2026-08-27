let target (x : int) = x
let premature (x : int) = target x
let wrapper (x : int) = target x
[@@verocaml.external_specification]
let later (x : int) = target x
