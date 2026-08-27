type wrapped = Wrapped of int Generic_family_provider.snapshot

let construction (stack : Generic_family_provider.Stack.t @ read) =
  Wrapped (Generic_family_provider.Stack.model stack)
