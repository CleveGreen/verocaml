type chain = Empty | Link of int * chain

let constructs (value : chain) : bool =
  match Link (0, value) with Empty -> true | Link _ -> false
[@@verocaml.spec]

let rec invalid_construction (value : chain) : bool =
  [%verocaml.decreases value];
  match value with
  | Empty -> constructs value
  | Link (_, rest) -> invalid_construction rest
[@@verocaml.spec] [@@verocaml.opaque]
