let predecessor (value : int) : int = value - 1 [@@verocaml.spec]

let rec helper_count (value : int) (remaining : int) : int =
  [%verocaml.decreases remaining];
  if remaining <= 0 then value
  else helper_count (value + 1) (predecessor remaining)
[@@verocaml.spec] [@@verocaml.revealed]

let rec inline_count (value : int) (remaining : int) : int =
  [%verocaml.decreases remaining];
  if remaining <= 0 then value
  else inline_count (value + 1) (remaining - 1)
[@@verocaml.spec] [@@verocaml.revealed]
