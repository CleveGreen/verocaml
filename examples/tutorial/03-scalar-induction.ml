let rec triangular (n : int) : int =
  [%verocaml.decreases n];
  if n <= 0 then 0 else n + triangular (n - 1)
[@@verocaml.spec] [@@verocaml.revealed]

let rec triangular_nonnegative (n : int) : unit =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun _result -> triangular n >= 0];
  [%verocaml.decreases n];
  if n = 0 then () else triangular_nonnegative (n - 1)
[@@verocaml.proof]

let use_theorem (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result = n];
  [%verocaml.proof triangular_nonnegative n];
  n
