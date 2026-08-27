let is_zero (value : int) : bool = value = 0 [@@verocaml.spec]

let is_nonpositive (value : int) : bool =
  is_zero value || value < 0
[@@verocaml.spec]

let rec count_up (value : int) (remaining : int) : int =
  [%verocaml.decreases remaining];
  if is_nonpositive remaining then value
  else count_up (value + 1) (remaining - 1)
[@@verocaml.spec] [@@verocaml.revealed]
