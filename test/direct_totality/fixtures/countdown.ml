let rec countdown (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result = 0];
  [%verocaml.decreases n];
  if n = 0 then 0 else countdown (n - 1)

let run_countdown (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result = 0];
  countdown n
