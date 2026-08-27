type node = Empty | Node of int * node

let receives_finite (_value : node [@finite]) : bool = true
[@@verocaml.spec]

let return_finite (value : node [@finite]) : node =
  [%verocaml.ensures fun result -> receives_finite result];
  value
