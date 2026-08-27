type node = Empty | Node of { value : int; mutable next : node }
type stack = { mutable top : node; mutable length : int }

let push_without_upper_bound (s : stack @ unique) value : stack @ unique =
  [%verocaml.requires s.length >= 0];
  [%verocaml.ensures fun result ->
    result.length = [%verocaml.old s.length] + 1];
  s.top <- Node { value; next = s.top };
  s.length <- s.length + 1;
  s
