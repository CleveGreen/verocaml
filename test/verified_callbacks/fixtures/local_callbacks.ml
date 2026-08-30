type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

let apply f x =
  [%verocaml.requires call_requires (f x)];
  [%verocaml.ensures fun result -> call_ensures (f x) result];
  f x

let immediate x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  apply
    (fun y ->
      [%verocaml.requires true];
      [%verocaml.ensures fun result -> result = y];
      y)
    x

let captured x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x || result = 0];
  let keep = x >= 0 in
  let callback y =
    [%verocaml.requires true];
    [%verocaml.ensures fun result -> result = y || result = 0];
    if keep then y else 0
  in
  let first = apply callback x in
  apply callback first

let generic_capture (x : int option) =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  let callback (y : int option) =
    [%verocaml.requires true];
    [%verocaml.ensures fun result -> result = x];
    let _ = y in
    match x with None -> None | Some value -> Some value
  in
  callback x

let shadowed_capture (x : int) =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  let keep = x in
  let callback (y : int) =
    [%verocaml.requires true];
    [%verocaml.ensures fun result -> result = keep];
    let _ = y in
    keep
  in
  let keep = false in
  let _ = keep in
  callback x

let identical_calls (x : int) =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x || result = 0];
  let nonnegative = x >= 0 in
  let callback (y : int) =
    [%verocaml.requires true];
    [%verocaml.ensures fun result -> result = y || result = 0];
    let _ = nonnegative in
    y
  in
  let first = callback x in
  let second = callback x in
  if first = x then second else first
