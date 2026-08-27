let consume (_value : Recursive_model_dependency.snapshot [@finite]) =
  ()

let finite_formal (stack : Recursive_model_dependency.Stack.t @ read) =
  consume (Recursive_model_dependency.Stack.model stack)
