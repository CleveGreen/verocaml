let raw_nonpositive (value : int) : bool = value <= 0

let rec invalid_exec (value : int) : int =
  [%verocaml.decreases value];
  if raw_nonpositive value then 0 else invalid_exec (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
