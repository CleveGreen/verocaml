let rec earlier_recursive (value : int) : bool =
  [%verocaml.decreases value];
  if value <= 0 then true else earlier_recursive (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let calls_distinct_recursive (value : int) : bool =
  earlier_recursive value
[@@verocaml.spec]

let rec invalid_distinct_recursive (value : int) : int =
  [%verocaml.decreases value];
  if calls_distinct_recursive value then 0
  else invalid_distinct_recursive (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
