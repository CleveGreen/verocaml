type wrapped = Wrapped of Recursive_model_dependency.snapshot

let construction (stack : Recursive_model_dependency.Stack.t @ read) =
  Wrapped (Recursive_model_dependency.Stack.model stack)
