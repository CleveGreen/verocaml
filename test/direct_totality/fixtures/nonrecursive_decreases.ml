let nonrecursive_decreases (n : int) =
  [%verocaml.decreases n];
  n
