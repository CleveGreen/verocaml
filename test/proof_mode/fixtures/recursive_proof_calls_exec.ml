let exec x = x + 1

let rec bad (n : int) : unit =
  [%verocaml.decreases n];
  if n = 0 then ()
  else (
    exec n;
    bad (n - 1))
[@@verocaml.proof]
