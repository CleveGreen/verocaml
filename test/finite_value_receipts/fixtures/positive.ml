type node = Empty | Node of int * node

type stack = {
  top : node;
  length : int;
}

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, tail) -> 1 + spec_node_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let spec_push_front value stack =
  Node (value, stack.top)
[@@verocaml.spec]

let spec_tail_len stack =
  match stack.top with
  | Empty -> 0
  | Node (_, tail) -> spec_node_len tail
[@@verocaml.spec]

let verified (choose : bool) =
  [%verocaml.assert
    let base = Node (2, Node (1, Empty)) in
    let stack = { top = base; length = 2 } in
    let alias = stack.top in
    let joined = if choose then alias else alias in
    spec_node_len joined = 2
    && spec_tail_len stack = 1
    && spec_node_len (spec_push_front 3 stack) = 3];
  ()

let local_exec_chain () =
  Node (2, Node (1, Empty))

let local_tracked_chain () : (node [@tracked]) =
  ((Node (2, Node (1, Empty))) [@tracked])
