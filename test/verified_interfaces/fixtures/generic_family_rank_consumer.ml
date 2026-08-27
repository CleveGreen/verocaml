let rec rank_use (stack : Generic_family_provider.Stack.t @ read) =
  [%verocaml.decreases (Generic_family_provider.Stack.model stack)];
  rank_use stack
