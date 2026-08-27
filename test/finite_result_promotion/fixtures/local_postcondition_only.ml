type node = Empty | Node of int * node

let rec spec_nonnegative (node : node) : bool =
  [%verocaml.decreases node];
  match node with Empty -> true | Node (_, next) -> spec_nonnegative next
[@@verocaml.spec] [@@verocaml.revealed]

let push_front value (xs : node [@finite]) : node =
  [%verocaml.ensures fun result -> spec_nonnegative result = spec_nonnegative result];
  Node (value, xs)
