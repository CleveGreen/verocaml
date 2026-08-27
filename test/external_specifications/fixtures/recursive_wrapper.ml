let target (x : int) = x
let rec wrapper (x : int) = target x
[@@verocaml.external_specification]
