type node = Empty | Node of int * node

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let direct_exec_reveal () =
  [%verocaml.reveal_with_fuel (spec_node_len, 1)]
