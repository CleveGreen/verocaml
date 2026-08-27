let consume (_value : int Generic_family_provider.snapshot [@finite]) =
  ()

let finite_formal (stack : Generic_family_provider.Stack.t @ read) =
  consume (Generic_family_provider.Stack.model stack)
