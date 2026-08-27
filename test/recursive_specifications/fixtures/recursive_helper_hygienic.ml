let reaches_base (value : int) : bool =
  let value = value - 1 in
  let shadow = value in
  shadow <= 0
[@@verocaml.spec]

let rec hygienic_count (value : int) (remaining : int) : int =
  [%verocaml.decreases remaining];
  let caller_local = remaining + 1 in
  if reaches_base caller_local then value
  else hygienic_count (value + 1) (remaining - 1)
[@@verocaml.spec] [@@verocaml.revealed]

let rec inline_hygienic_count (value : int) (remaining : int) : int =
  [%verocaml.decreases remaining];
  let caller_local = remaining + 1 in
  if
    let value = caller_local - 1 in
    let shadow = value in
    shadow <= 0
  then value
  else inline_hygienic_count (value + 1) (remaining - 1)
[@@verocaml.spec] [@@verocaml.revealed]
