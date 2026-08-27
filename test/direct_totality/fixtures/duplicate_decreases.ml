let rec duplicate_decreases (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases n];
  [%verocaml.decreases n - 1];
  if n = 0 then 0 else duplicate_decreases (n - 1)
