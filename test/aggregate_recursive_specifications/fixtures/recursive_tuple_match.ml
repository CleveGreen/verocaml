type 'a node =
  | Empty
  | Node of 'a * 'a node

let rec spec_node_eq_alt (a : int node) (b : int node) : bool =
  [%verocaml.decreases a];
  match a, b with
  | Empty, Empty -> true
  | Node (va, na), Node (vb, nb) ->
      va = vb && spec_node_eq_alt na nb
  | _, _ -> false
[@@verocaml.spec]
[@@verocaml.revealed]

let spec_payload_eq (left : int) (right : int) : bool =
  left = right
[@@verocaml.spec]

let rec spec_node_eq_with_helper (a : int node) (b : int node) : bool =
  [%verocaml.decreases a];
  spec_payload_eq 0 0
  && match a, b with
     | Empty, Empty -> true
     | Node (va, na), Node (vb, nb) ->
         spec_payload_eq va vb && spec_node_eq_with_helper na nb
     | _, _ -> false
[@@verocaml.spec]
[@@verocaml.revealed]

let rec spec_nested_tuple_match
    (a : int node)
    (b : int node)
    (flag : bool)
    (count : int) : bool =
  [%verocaml.decreases a];
  match (~left:(a, flag), ~right:(b, count)) with
  | (~left:(Empty, true), ~right:(Empty, 0)) -> true
  | (~left:(Node (va, na), enabled),
     ~right:(Node (vb, nb), remaining))
    when enabled && remaining >= 0 ->
      va = vb && spec_nested_tuple_match na nb enabled (remaining - 1)
  | (~left:(_, _), ~right:(_, _)) -> false
[@@verocaml.spec]
[@@verocaml.revealed]

let spec_is_empty (node : int node) : bool =
  match node with Empty -> true | Node _ -> false
[@@verocaml.spec]

let check_empty_pair
    (a : int node [@finite])
    (b : int node [@finite]) : unit =
  [%verocaml.requires spec_is_empty a && spec_is_empty b];
  [%verocaml.reveal_with_fuel (spec_node_eq_alt, 1)];
  match a with
  | Empty ->
      (match b with
       | Empty -> [%verocaml.assert spec_node_eq_alt a b]
       | Node _ -> [%verocaml.assert false])
  | Node _ -> [%verocaml.assert false]
[@@verocaml.proof]
