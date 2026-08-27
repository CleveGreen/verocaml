type 'a choice = First of 'a | Second of 'a
let wrong (value : int choice) =
  [%verocaml.ensures fun result -> result];
  match value with First _ -> value = Second 0 | Second _ -> true
