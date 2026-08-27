let rec non_strict (n : int) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases n];
  if n = 0 then () else non_strict n
[@@verocaml.proof]
