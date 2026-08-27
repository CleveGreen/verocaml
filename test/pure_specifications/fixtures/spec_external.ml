let bad (x : int) : int =
  print_int x;
  x
[@@verocaml.spec]
