type marker = Stop | Continue

let marker_is_stop (value : marker) : bool =
  match value with Stop -> true | Continue -> false
[@@verocaml.spec]

let rec invalid_helper_only_aggregate (value : int) : int =
  [%verocaml.decreases value];
  if marker_is_stop Stop then 0
  else invalid_helper_only_aggregate (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
