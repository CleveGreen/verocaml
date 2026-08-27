type 'a node = Empty | Node of 'a * 'a node

type stack = {
  top : int node;
  length : int;
}

let spec_push_front (value : int) (stack : stack) : stack =
  let { top; length } = stack in
  { top = Node (value, top); length = length + 1 }
[@@verocaml.spec]

let rec spec_node_eq (a : int node) (b : int node) : bool =
  [%verocaml.decreases a];
  match a with
  | Empty -> (match b with Empty -> true | Node _ -> false)
  | Node (va, na) ->
      (match b with
       | Node (vb, nb) -> va = vb && spec_node_eq na nb
       | Empty -> false)
[@@verocaml.spec]
[@@verocaml.revealed]

let rec node_eq_refl (node : int node [@finite]) : unit =
  [%verocaml.ensures fun _result -> spec_node_eq node node];
  [%verocaml.decreases node];
  match node with
  | Empty -> ()
  | Node (_, next) -> node_eq_refl next
[@@verocaml.proof]

let establish_node_eq_refl (node : int node [@finite]) : unit =
  [%verocaml.ensures fun _result -> spec_node_eq node node];
  [%verocaml.proof node_eq_refl node];
  ()
