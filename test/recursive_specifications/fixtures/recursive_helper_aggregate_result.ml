type chain = Empty | Link of int * chain

let tail (value : chain) : chain =
  match value with Empty -> Empty | Link (_, rest) -> rest
[@@verocaml.spec]

let rec invalid_helper_result (value : chain) : int =
  [%verocaml.decreases value];
  match value with
  | Empty -> 0
  | Link (_, rest) ->
      (match tail rest with
      | Empty -> 1
      | Link _ -> invalid_helper_result rest)
[@@verocaml.spec] [@@verocaml.opaque]
