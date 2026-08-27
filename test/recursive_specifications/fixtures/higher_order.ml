let rec bad (f : int -> int) (n : int) : int =
  [%verocaml.decreases n];
  if n <= 0 then f n else bad f (n - 1)
[@@verocaml.spec] [@@verocaml.opaque]
