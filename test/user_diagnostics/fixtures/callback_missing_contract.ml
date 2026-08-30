let apply (callback : int -> int) (value : int) : int =
  [%verocaml.requires call_requires (callback value)];
  [%verocaml.ensures fun result -> call_ensures (callback value) result];
  callback value

let rejected (value : int) : int =
  let callback (argument : int) : int = argument in
  apply callback value
