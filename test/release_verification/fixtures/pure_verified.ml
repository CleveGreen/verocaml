let safe_increment x =
  [%verocaml.requires x < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result -> result = x + 1];
  [%verocaml.assert x + 1 <= 4_611_686_018_427_387_903];
  x + 1

let rec countdown n =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases n];
  if n = 0 then 0 else countdown (n - 1)
