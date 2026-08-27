let apply (f : 'a -> 'a) (x : 'a) =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let relay (callback : 'a -> 'a) value =
  [%verocaml.requires call_requires (callback value)];
  [%verocaml.ensures fun result ->
    call_ensures (callback value) result];
  callback value

let identity x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  x

let negate x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = not x];
  not x

let use_int x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  apply identity x

let use_bool x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = not x];
  relay negate x
