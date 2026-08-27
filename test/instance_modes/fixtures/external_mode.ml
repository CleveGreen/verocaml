let target value = value

let wrapper (value : int [@ghost]) : (int [@ghost]) =
  target value
[@@verocaml.external_specification]
