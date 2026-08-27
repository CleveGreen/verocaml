let rec overflowing_measure (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.decreases n + 1];
  if n = 0 then 0 else overflowing_measure (n - 1)
