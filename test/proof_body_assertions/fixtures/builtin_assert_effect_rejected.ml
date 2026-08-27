let effect_rejected (condition : bool) : unit =
  assert (print_endline "effect"; condition)
