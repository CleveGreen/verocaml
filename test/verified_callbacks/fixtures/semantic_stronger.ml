let choose x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x || result = 0];
  if x = 0 then 0 else x

let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let client x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  apply choose x
