let rec left (n : int) : int =
  [%verocaml.decreases n];
  if n <= 0 then n else right (n - 1)
[@@verocaml.spec] [@@verocaml.opaque]

and right (n : int) : int =
  [%verocaml.decreases n];
  if n <= 0 then n else left (n - 1)
[@@verocaml.spec] [@@verocaml.opaque]
