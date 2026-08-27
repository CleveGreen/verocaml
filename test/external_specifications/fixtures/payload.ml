let target (x : int) = x
let wrapper (x : int) = target x
[@@verocaml.external_specification "target"]
