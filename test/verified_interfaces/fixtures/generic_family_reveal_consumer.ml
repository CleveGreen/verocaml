let reveal_use (stack : Generic_family_provider.Stack.t @ read) =
  [%verocaml.reveal_with_fuel
    (Generic_family_provider.Stack.model, 1)];
  Generic_family_provider.Stack.model stack
