let labelled ~value : bool = value <= 0 [@@verocaml.spec]

let rec invalid_labelled (value : int) : int =
  [%verocaml.decreases value];
  if labelled ~value then 0 else invalid_labelled (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
