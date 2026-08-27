type node = Empty | Node of { value : int; mutable next : node }
type stack = { mutable top : node; mutable length : int }

let drop (s : stack @ unique) : stack @ unique =
  [%verocaml.requires
    s.length > 0
    && (match s.top with
        | Empty -> false
        | Node { value = _; next = _ } -> true)];
  [%verocaml.ensures fun result ->
    result.length = [%verocaml.old s.length] - 1];
  match s.top with
  | Empty -> s
  | Node { value = _; next } ->
      s.top <- next;
      s.length <- s.length - 1;
      s

let call_drop_without_nonemptiness (s : stack @ unique) : stack @ unique =
  [%verocaml.requires s.length >= 0];
  let s = drop s in
  s
