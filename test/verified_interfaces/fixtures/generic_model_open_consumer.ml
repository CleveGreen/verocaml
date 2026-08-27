let observe (stack : Generic_model_dependency.Stack.t @ read) (value : 'a) =
  let _ = value in
  Generic_model_dependency.Stack.model stack
