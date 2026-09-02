let rec tuple_result (n : Vstd.Int.t) : Vstd.Int.t * Vstd.Int.t =
  [%verocaml.decreases n];
  if n <= 0 then (0, 0) else tuple_result (n - 1)
[@@verocaml.spec]
[@@verocaml.opaque]
