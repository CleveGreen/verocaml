let rec duplicate (n : int) : unit =
  [%verocaml.decreases n];
  [%verocaml.decreases n];
  if n = 0 then () else duplicate (n - 1)
[@@verocaml.proof]
