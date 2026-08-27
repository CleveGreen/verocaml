let rec equal
    (left : Recursive_model_dependency.snapshot)
    (right : Recursive_model_dependency.snapshot) =
  [%verocaml.decreases left];
  match left with
  | Recursive_model_dependency.End ->
      (match right with
      | Recursive_model_dependency.End -> true
      | Recursive_model_dependency.More _ -> false)
  | Recursive_model_dependency.More (value, tail) ->
      (match right with
      | Recursive_model_dependency.End -> false
      | Recursive_model_dependency.More (other, rest) ->
          value = other && equal tail rest)
[@@verocaml.spec]
[@@verocaml.revealed]

let recursive_use (stack : Recursive_model_dependency.Stack.t @ read) =
  equal
    (Recursive_model_dependency.Stack.model stack)
    (Recursive_model_dependency.Stack.model stack)
