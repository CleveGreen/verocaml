type node =
  | Empty
  | Node of { value : int; mutable next : node }

type stack = { mutable top : node; mutable length : int }

let keep_only_first (s : stack @ unique) : stack @ unique =
  [%verocaml.requires
    s.length > 0
    && match s.top with Empty -> false | Node { value = _; next = _ } -> true];
  [%verocaml.ensures fun result ->
    result.length = 1
    && match result.top with
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

let caller (s : stack @ unique) =
  [%verocaml.requires
    s.length > 0
    && match s.top with Empty -> false | Node { value = _; next = _ } -> true];
  [%verocaml.ensures fun result -> result = 1];
  let s = keep_only_first s in
  s.length
