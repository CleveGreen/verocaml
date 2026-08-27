[@@@verocaml.verify]

let labelled_specification ~(first : int @ local) ~second =
  Curried_legacy.labelled ~first ~second
[@@verocaml.external_specification]
