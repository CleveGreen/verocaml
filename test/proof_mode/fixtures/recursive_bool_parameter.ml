let rec bool_induct (n : Vstd.Int.t) (flag : bool) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases n];
  if n = 0 then () else bool_induct (n - 1) (not flag)
[@@verocaml.proof]
