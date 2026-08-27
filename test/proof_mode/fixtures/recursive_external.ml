let rec bad (n : int) : unit =
  [%verocaml.decreases n];
  if n = 0 then ()
  else (
    print_int n;
    bad (n - 1))
[@@verocaml.proof]
