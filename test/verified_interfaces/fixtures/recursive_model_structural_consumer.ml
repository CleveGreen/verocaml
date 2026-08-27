let structural (stack : Recursive_model_dependency.Stack.t @ read) =
  match Recursive_model_dependency.Stack.model stack with
  | Recursive_model_dependency.End -> 0
  | Recursive_model_dependency.More _ -> 1
