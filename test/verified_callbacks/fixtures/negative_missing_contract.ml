let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let rejected (x : int) =
  let callback (y : int) = y in
  apply callback x
