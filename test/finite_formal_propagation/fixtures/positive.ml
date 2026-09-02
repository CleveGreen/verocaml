type node = Empty | Node of int * node

type stack = {
  top : node;
  length : int;
}

let rec spec_node_len (node : node) : Vstd.Int.t =
  [%verocaml.decreases node];
  match node with
  | Empty -> 0
  | Node (_, tail) -> 1 + abs (spec_node_len tail)
[@@verocaml.spec] [@@verocaml.revealed]

let checked (choose : bool) (left : node [@finite])
    (right : node [@finite]) =
  [%verocaml.assert
    let selected = if choose then left else right in
    spec_node_len selected >= 0];
  ()

let one (value : node [@finite]) =
  [%verocaml.assert spec_node_len value = spec_node_len value];
  ()

let tracked () (value : node [@tracked] [@finite]) =
  [%verocaml.assert spec_node_len value = spec_node_len value];
  ()

let caller (choose : bool) =
  let left = Node (1, Empty) in
  let right = Node (2, Empty) in
  let alias = left in
  let stack = { top = right; length = 1 } in
  checked true alias stack.top;
  checked false left right;
  one (if choose then left else right);
  tracked () ((Node (3, Empty)) [@tracked])
