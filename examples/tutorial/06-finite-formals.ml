type node = Empty | Node of int * node

let rec length (node : node) : int =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, tail) -> 1 + length tail
[@@verocaml.spec] [@@verocaml.revealed]

let push_front value (tail : node [@finite]) : node =
  [%verocaml.ensures fun result -> length result = length tail + 1];
  Node (value, tail)

let rec inspect (node : node [@finite]) : unit =
  [%verocaml.ensures fun _result -> length node >= 0];
  [%verocaml.decreases node];
  match node with
  | Empty -> ()
  | Node (_, tail) -> inspect tail
[@@verocaml.proof]

let demonstrate_receipts () =
  [%verocaml.ensures fun result -> result = 0];
  let locally_built = Node (2, Node (1, Empty)) in
  [%verocaml.proof inspect locally_built];
  let exact_result = push_front 3 locally_built in
  [%verocaml.proof inspect exact_result];
  match exact_result with Empty -> 0 | Node _ -> 0
