let bad x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  if x = 0 then 1 else x

let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let client x = apply bad x
