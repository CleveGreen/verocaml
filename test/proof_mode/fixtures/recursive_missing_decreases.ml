let rec missing (n : int) : unit =
  if n = 0 then () else missing (n - 1)
[@@verocaml.proof]
