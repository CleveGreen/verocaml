let runtime_guard (condition : bool) : unit =
  [%verocaml.requires condition];
  assert condition

let retained_check (condition : bool) : unit =
  assert (condition = condition)
