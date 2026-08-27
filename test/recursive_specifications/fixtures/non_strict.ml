let rec recurse (x : int) (y : int) : int =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) y
[@@verocaml.spec] [@@verocaml.opaque]
