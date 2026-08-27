let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let rejected x =
  let captured = ref x in
  let callback y =
    [%verocaml.requires true];
    [%verocaml.ensures fun _ -> true];
    y + !captured
  in
  apply callback x
