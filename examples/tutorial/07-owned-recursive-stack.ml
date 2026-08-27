type node =
  | Empty
  | Node of {
      value : int;
      mutable next : node;
    }

type stack = {
  mutable top : node;
  mutable length : int;
}

let empty () : stack @ unique =
  [%verocaml.ensures fun result ->
    result.length = 0
    &&
    match result.top with
    | Empty -> true
    | Node { value = _; next = _ } -> false];
  { top = Empty; length = 0 }

let push (stack : stack @ unique) (value : int) : stack @ unique =
  [%verocaml.requires
    stack.length >= 0
    && stack.length < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result.length = [%verocaml.old stack.length] + 1
    &&
    match result.top with
    | Empty -> false
    | Node { value = _; next = _ } -> true];
  stack.top <- Node { value; next = stack.top };
  stack.length <- stack.length + 1;
  stack

let drop (stack : stack @ unique) : stack @ unique =
  [%verocaml.requires
    stack.length > 0
    &&
    match stack.top with
    | Empty -> false
    | Node { value = _; next = _ } -> true];
  [%verocaml.ensures fun result ->
    result.length = [%verocaml.old stack.length] - 1];
  match stack.top with
  | Empty -> stack
  | Node { value = _; next } ->
      stack.top <- next;
      stack.length <- stack.length - 1;
      stack

let rec drain (stack : stack @ unique) : stack @ unique =
  [%verocaml.requires stack.length >= 0];
  [%verocaml.ensures fun result ->
    result.length = 0
    &&
    match result.top with
    | Empty -> true
    | Node { value = _; next = _ } -> false];
  [%verocaml.decreases stack.length];
  if stack.length = 0 then (
    stack.top <- Empty;
    stack)
  else
    match stack.top with
    | Empty ->
        stack.length <- 0;
        stack
    | Node { value = _; next } ->
        stack.top <- next;
        stack.length <- stack.length - 1;
        drain stack

let push_drop_drain value =
  let stack = empty () in
  let stack = push stack value in
  let stack = drop stack in
  let stack = push stack value in
  let stack = drain stack in
  stack.length
