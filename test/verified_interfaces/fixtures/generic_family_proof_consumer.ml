let direct_proof (stack : Generic_family_provider.Stack.t @ read) =
  Generic_family_provider.int_reflexive_control
    (Generic_family_provider.Stack.model stack)
