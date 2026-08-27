type chain = End | Next of chain

let rec recurse value =
  [%verocaml.decreases value];
  match value with End -> 0 | Next rest -> recurse rest
[@@verocaml.spec] [@@verocaml.opaque]

let recursion_cycle (value : chain) =
  [%verocaml.ensures fun result -> recurse value = result];
  0
