type chain = Empty | Link of int * chain

let invariant (value : chain) : bool =
  match value with Empty -> true | Link _ -> true
[@@verocaml.type_invariant]

let rec invalid_invariant_helper (value : chain) : bool =
  [%verocaml.decreases value];
  match value with
  | Empty -> invariant value
  | Link (_, rest) -> invalid_invariant_helper rest
[@@verocaml.spec] [@@verocaml.opaque]
