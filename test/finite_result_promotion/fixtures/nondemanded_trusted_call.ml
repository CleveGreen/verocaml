type node = Empty | Node of int * node

let trusted_make (value : int) : node =
  [%verocaml.ensures fun _result -> true];
  Node (value, Empty)
[@@verocaml.external_body]

let run () =
  let _unused = trusted_make 0 in
  0
