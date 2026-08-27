let rec bad (n : int) : int =
  [%verocaml.decreases n];
  if n <= 0 then n
  else
    let () = print_int n in
    bad (n - 1)
[@@verocaml.spec] [@@verocaml.opaque]
