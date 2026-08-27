let trusted_nonpositive (value : int) : bool =
  [%verocaml.ensures fun result -> result = (value <= 0)];
  value <= 0
[@@verocaml.external_body]

let rec invalid_trusted (value : int) : int =
  [%verocaml.decreases value];
  if trusted_nonpositive value then 0 else invalid_trusted (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
