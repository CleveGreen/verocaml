let invariant_use (stack : Recursive_model_dependency.Stack.t @ read) =
  [%verocaml.proof
    [%verocaml.use_type_invariant
      (Recursive_model_dependency.Stack.model stack)]]
