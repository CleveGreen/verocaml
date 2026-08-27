let tuple_helper (value : int) : bool =
  let pair = (value, value) in
  pair = (0, 0)
[@@verocaml.spec]

let rec invalid_tuple (value : int) : int =
  [%verocaml.decreases value];
  if tuple_helper value then 0 else invalid_tuple (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
