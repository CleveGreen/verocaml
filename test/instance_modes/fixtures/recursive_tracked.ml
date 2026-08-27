let rec bad (x : int [@tracked]) : unit =
  [%verocaml.decreases x];
  if x <= 0 then () else bad (x - 1)
[@@verocaml.proof]
