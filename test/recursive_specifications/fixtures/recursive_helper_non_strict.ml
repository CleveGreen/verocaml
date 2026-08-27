let unchanged (value : int) : int = value [@@verocaml.spec]

let rec non_strict_helper (value : int) : int =
  [%verocaml.decreases value];
  if value <= 0 then 0 else non_strict_helper (unchanged value)
[@@verocaml.spec] [@@verocaml.opaque]
