let integer_branch (value : int) : int =
  if value <= 0 then 0 else value
[@@verocaml.spec]

let rec invalid_integer_branch (value : int) : int =
  [%verocaml.decreases value];
  if integer_branch value = 0 then 0
  else invalid_integer_branch (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
