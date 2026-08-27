let identity x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  x

let select ~acc ~item =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = acc || result = item];
  if item >= 0 then acc else item

let zero_or_self x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x || result = 0];
  if x >= 0 then x else 0

let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let apply_labelled (f : acc:int -> item:int -> int) acc item =
  [%verocaml.requires call_requires (f ~item ~acc)];
  [%verocaml.ensures fun result -> call_ensures (f ~item ~acc) result];
  f ~item ~acc

let use_identity x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  apply identity x

let use_zero_or_self x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x || result = 0];
  apply zero_or_self x

let use_labelled acc item =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = acc || result = item];
  apply_labelled select acc item
