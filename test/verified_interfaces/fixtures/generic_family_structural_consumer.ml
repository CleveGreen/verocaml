let structural (stack : Generic_family_provider.Stack.t @ read) =
  match Generic_family_provider.Stack.model stack with
  | Generic_family_provider.End -> 0
  | Generic_family_provider.More _ -> 1
