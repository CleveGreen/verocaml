let apply predicate value = predicate value [@@verocaml.spec]

let nonpositive (value : int) : bool = value <= 0 [@@verocaml.spec]

let rec invalid_higher_order (value : int) : int =
  [%verocaml.decreases value];
  if apply nonpositive value then 0
  else invalid_higher_order (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
