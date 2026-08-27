let apply (function_value : int -> int) (value : int) = function_value value
[@@verocaml.spec]

let lambda_higher_order (value : int) =
  [%verocaml.ensures fun result -> apply (fun item -> item) value = result];
  value
