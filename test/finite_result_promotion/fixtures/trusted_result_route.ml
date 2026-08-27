type node = Empty | Node of int * node

let trusted_make value =
  [%verocaml.ensures fun _result -> true];
  Node (value, Empty)
[@@verocaml.external_body]

let consume (_node : node [@finite]) : unit = ()

let use () = consume (trusted_make 0)
