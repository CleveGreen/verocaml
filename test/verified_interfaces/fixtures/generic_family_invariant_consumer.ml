let invariant_use (stack : Generic_family_provider.Stack.t @ read) =
  [%verocaml.proof
    [%verocaml.use_type_invariant
      (Generic_family_provider.Stack.model stack)]]
