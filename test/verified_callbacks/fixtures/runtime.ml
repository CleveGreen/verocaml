let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let identity x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  x

let () =
  print_int (apply identity 17);
  print_newline ()
