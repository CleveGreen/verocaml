let both_nonpositive (left : int) (right : int) : bool =
  left <= 0 && right <= 0
[@@verocaml.spec]

let apply predicate value = predicate value [@@verocaml.spec]

let rec invalid_partial (value : int) : int =
  [%verocaml.decreases value];
  if apply (both_nonpositive value) value then 0
  else invalid_partial (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
