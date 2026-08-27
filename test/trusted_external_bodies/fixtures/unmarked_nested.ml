type node = Empty | Node of { value : int; mutable next : node }
type stack = { mutable top : node; mutable length : int }
let keep_only_first (s : stack @ unique) : stack @ unique =
  [%verocaml.ensures fun result -> result.length = 1];
  match s.top with
  | Empty -> s
  | Node record -> record.next <- Empty; s.length <- 1; s
