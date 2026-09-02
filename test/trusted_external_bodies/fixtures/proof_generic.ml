let trusted x =
  [%verocaml.ensures fun _ -> false];
  ignore x
[@@verocaml.external_body]
[@@verocaml.proof]

let instantiate_integer () = trusted 0
[@@verocaml.proof]

let instantiate_boolean () = trusted true
[@@verocaml.proof]
