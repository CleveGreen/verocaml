let rec bad f (n : int) : unit =
  [%verocaml.decreases n];
  if n = 0 then ()
  else (
    f n;
    bad f (n - 1))
[@@verocaml.proof]
