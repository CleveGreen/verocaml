let rec negative (n : Vstd.Int.t) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases -1];
  if n = 0 then () else negative (n - 1)
[@@verocaml.proof]
