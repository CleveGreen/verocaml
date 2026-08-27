let explicit_assert_failure (condition : bool) : unit =
  if condition then ()
  else raise (Assert_failure ("builtin_assert_lookalike.ml", 3, 7))
