let target x = x
let wrapper x = target x [@@verocaml.external_specification]
and other x = x
