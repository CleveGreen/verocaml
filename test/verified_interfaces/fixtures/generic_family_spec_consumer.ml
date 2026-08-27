let direct_nonmodel_spec (stack : Generic_family_provider.Stack.t @ read) =
  Generic_family_provider.int_equal_control
    (Generic_family_provider.Stack.model stack)
