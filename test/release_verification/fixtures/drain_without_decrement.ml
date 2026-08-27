type node = Empty | Node of { value : int; mutable next : node }
type stack = { mutable top : node; mutable length : int }

let rec drain_without_decrement (s : stack @ unique) : stack @ unique =
  [%verocaml.requires s.length >= 0];
  [%verocaml.ensures fun result -> result.length >= 0];
  [%verocaml.decreases s.length];
  if s.length = 0 then s else drain_without_decrement s
