let factory (value : int) : int -> int = fun _ -> value [@@verocaml.spec]

let pointwise (value : int) : int =
  [%verocaml.assert (fun argument -> argument + 1) = (fun argument -> 1 + argument)];
  [%verocaml.ensures fun result -> result = value];
  value

let captures (left : int) (right : int) : int =
  [%verocaml.assert factory left <> factory right];
  [%verocaml.ensures fun result -> result = left];
  left
