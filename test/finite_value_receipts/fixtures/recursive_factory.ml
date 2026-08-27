type node = Empty | Node of int * node

module rec Cycle : sig
  val make : int -> node
end = struct
  let make value =
    let rec cycle = Node (value, cycle) in
    cycle
end

let rec spec_node_len node =
  [%verocaml.decreases node];
  match node with Empty -> 0 | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let verified () =
  [%verocaml.assert spec_node_len (Cycle.make 0) = 1];
  ()
