type node = Empty | Node of int * node

let make (value : int) =
  [%verocaml.ensures fun _result -> value = value];
  Node (value, Empty)
[@@verocaml.external_body]
