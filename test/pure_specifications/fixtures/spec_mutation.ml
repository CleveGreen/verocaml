let bad (x : int) : int =
  let mutable y = x in
  y <- y + 1;
  y
[@@verocaml.spec]
