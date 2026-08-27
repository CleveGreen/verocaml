let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let rejected x =
  let rec callback y =
    [%verocaml.requires true];
    [%verocaml.ensures fun _ -> true];
    if y = 0 then 0 else callback 0
  in
  apply callback x
