let rec left (n : int) : unit =
  [%verocaml.decreases n];
  if n = 0 then () else right (n - 1)
[@@verocaml.proof]

and right (n : int) : unit =
  [%verocaml.decreases n];
  if n = 0 then () else left (n - 1)
[@@verocaml.proof]
