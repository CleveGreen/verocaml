let reveal_use (stack : Recursive_model_dependency.Stack.t @ read) =
  [%verocaml.reveal_with_fuel
    (Recursive_model_dependency.Stack.model, 1)];
  Recursive_model_dependency.Stack.model stack
