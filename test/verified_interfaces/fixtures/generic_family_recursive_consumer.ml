let rec equal
    (left : int Generic_family_provider.snapshot)
    (right : int Generic_family_provider.snapshot) =
  [%verocaml.decreases left];
  match left with
  | Generic_family_provider.End ->
      (match right with
      | Generic_family_provider.End -> true
      | Generic_family_provider.More _ -> false)
  | Generic_family_provider.More (value, tail) ->
      (match right with
      | Generic_family_provider.End -> false
      | Generic_family_provider.More (other, rest) ->
          value = other && equal tail rest)
[@@verocaml.spec]
[@@verocaml.revealed]

let recursive_use (stack : Generic_family_provider.Stack.t @ read) =
  equal
    (Generic_family_provider.Stack.model stack)
    (Generic_family_provider.Stack.model stack)
