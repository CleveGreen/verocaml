let source_nonpositive (value : int) : bool = value <= 0

let external_nonpositive (value : int) : bool =
  [%verocaml.ensures fun result -> result = (value <= 0)];
  source_nonpositive value
[@@verocaml.external_specification]

let rec invalid_external (value : int) : int =
  [%verocaml.decreases value];
  if external_nonpositive value then 0 else invalid_external (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
