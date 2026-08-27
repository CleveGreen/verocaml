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

let push (s : stack @ unique) (value : int) : stack @ unique =
  [%verocaml.requires
    s.length >= 0 && s.length < 4_611_686_018_427_387_903];
  [%verocaml.ensures fun result ->
    result.length = [%verocaml.old s.length] + 1
    && (match result.top with Empty -> false | Node { value = _; next = _ } -> true)];
  s.top <- Node { value; next = s.top };
  s.length <- s.length + 1;
  s

let keep_only_first (s : stack @ unique) : stack @ unique =
  [%verocaml.requires
    s.length > 0
    && (match s.top with Empty -> false | Node { value = _; next = _ } -> true)];
  [%verocaml.ensures fun result ->
    result.length = 1
    &&
    match result.top with
    | Empty -> false
    | Node { value = _; next = Empty } -> true
    | Node { value = _; next = Node { value = _; next = _ } } -> false];
  match s.top with
  | Empty -> s
  | Node record ->
      record.next <- Empty;
      s.length <- 1;
      s
[@@verocaml.external_body]

let keep_only_first_and_read (s : stack @ unique) =
  [%verocaml.requires
    s.length > 0
    && (match s.top with Empty -> false | Node { value = _; next = _ } -> true)];
  [%verocaml.ensures fun result -> result = 1];
  let s = keep_only_first s in
  s.length

let drop (s : stack @ unique) : stack @ unique =
  [%verocaml.requires
    s.length > 0
    && (match s.top with Empty -> false | Node { value = _; next = _ } -> true)];
  [%verocaml.ensures fun result ->
    result.length = [%verocaml.old s.length] - 1];
  match s.top with
  | Empty -> s
  | Node { value = _; next } ->
      s.top <- next;
      s.length <- s.length - 1;
      s

let rec drain (s : stack @ unique) : stack @ unique =
  [%verocaml.requires s.length >= 0];
  [%verocaml.ensures fun result ->
    result.length = 0
    && (match result.top with Empty -> true | Node { value = _; next = _ } -> false)
    && [%verocaml.old s.length] >= 0];
  [%verocaml.decreases s.length];
  if s.length = 0 then (
    s.top <- Empty;
    s)
  else
    match s.top with
    | Empty ->
        s.length <- 0;
        s
    | Node { value = _; next } ->
        s.top <- next;
        s.length <- s.length - 1;
        drain s

let drain_and_read (s : stack @ unique) =
  [%verocaml.requires s.length >= 0];
  [%verocaml.ensures fun result -> result = 0];
  let s = drain s in
  s.length
