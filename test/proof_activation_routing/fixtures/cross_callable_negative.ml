type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec spec_node_len (node : int node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec] [@@verocaml.opaque]

let affirm (condition : bool) =
  [%verocaml.requires condition];
  ()
[@@verocaml.proof]

let revealing_callee (_dummy : bool) =
  [%verocaml.reveal_with_fuel (spec_node_len, 2)]
[@@verocaml.proof]

let cross_callable_nonleak (_dummy : bool) =
  revealing_callee true;
  affirm (spec_node_len Empty = 0)
[@@verocaml.proof]
