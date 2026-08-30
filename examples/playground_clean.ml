let implies premise conclusion = (not premise) || conclusion
[@@verocaml.spec]

let iff a b = a = b
[@@verocaml.spec]

let lemma_iff a b =
  [%verocaml.ensures fun _ ->
    iff
      (iff a b)
      ((implies a b) && (implies b a))
  ];
  let _ = (a && b) in
  ()
[@@verocaml.proof]

let admit () =
  [%verocaml.ensures fun _ -> false];
  ()
[@@verocaml.axiom]

let assume (cond : bool) =
  [%verocaml.ensures fun _ -> cond];
  admit ()
[@@verocaml.proof]

let affirm (cond : bool) =
  [%verocaml.requires cond];
  ()
[@@verocaml.proof]

let proclaim (cond : bool) =
  [%verocaml.requires cond];
  [%verocaml.ensures fun _ -> cond];
  ()
[@@verocaml.proof]

let impossible (cond : bool) =
  [%verocaml.requires (not cond)];
  [%verocaml.ensures fun _ -> (not cond) || false];
  ()
[@@verocaml.proof]

type 'a node =
  | Empty
  | Node of 'a * 'a node

type 'a stack = {
  top : 'a node;
  length : int;
}

let spec_is_empty a : bool =
  match a with
    | Empty -> true
    | Node _ -> false
[@@verocaml.spec]

let rec spec_node_len (node : 'a node) : int =
  [%verocaml.decreases node];
  match node with
    | Empty -> 0
    | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec]
[@@verocaml.revealed]

let lemma_empty_is_empty (empty : 'a node) =
  [%verocaml.requires empty = Empty];
  [%verocaml.ensures fun _ -> spec_is_empty empty];
  ()
[@@verocaml.proof]

let lemma_non_empty_is_not_empty (non_empty : 'a node) =
  [%verocaml.requires non_empty <> Empty];
  [%verocaml.ensures fun _ -> not (spec_is_empty non_empty)];
  match non_empty with
    | Empty -> assert false
    | Node _ -> ()
[@@verocaml.proof]

let lemma_empty_converse (empty : 'a node) =
  [%verocaml.requires spec_is_empty empty];
  [%verocaml.ensures fun _ -> empty = Empty];
  if empty <> Empty then begin
    lemma_non_empty_is_not_empty empty;
    assert false
  end else
    ()
[@@verocaml.proof]

let lemma_empty_len (node : 'a node [@finite]) =
  [%verocaml.requires spec_is_empty node];
  [%verocaml.ensures fun _ -> (spec_node_len node) = 0];
  [%verocaml.reveal_with_fuel (spec_node_len, 1)];
  ()
[@@verocaml.proof]

let spec_push_front (value : int) (stack : int stack) : int stack =
  let { top; length; } = stack in
  { top = Node (value, top); length = length + 1 }
[@@verocaml.spec]

let rec spec_node_eq (a : 'a node) (b : 'a node) : bool =
  [%verocaml.decreases a];
  match (a, b) with
    | (Empty, Empty) -> true
    | (Node (va, na), Node(vb, nb)) -> va = vb && spec_node_eq na nb
    | (_, _) -> false
[@@verocaml.spec]
[@@verocaml.revealed]

let rec node_eq_symm
    (a : 'a node [@finite])
    (b : 'a node [@finite]) =
  [%verocaml.requires spec_node_eq a b];
  [%verocaml.ensures fun _ -> spec_node_eq b a];
  [%verocaml.decreases a];
  match (a, b) with
    | (Node (va, na), Node(vb, nb)) -> node_eq_symm na nb
    | (_, _) -> ()
[@@verocaml.proof]

(*
let rec node_eq_trans
    (a : int node [@finite])
    (b : int node [@finite])
    (c : int node [@finite]) =
  [%verocaml.requires spec_node_eq a b];
  [%verocaml.requires spec_node_eq b c];
  [%verocaml.ensures fun _ -> spec_node_eq a c];
  [%verocaml.decreases a];
  match a with
  | Empty ->
      begin match b with
      | Empty ->
          begin match c with
          | Empty -> ()
          | Node (_, _) -> ()
          end
      | Node (_, _) -> ()
      end
  | Node (va, na) ->
      begin match b with
      | Empty -> ()
      | Node (vb, nb) ->
          begin match c with
          | Empty -> ()
          | Node (vc, nc) ->
              node_eq_trans na nb nc
          end
      end
[@@verocaml.proof]

let rec spec_node_eq_alt (a : int node) (b : int node) : bool =
  [%verocaml.decreases a];
  match a, b with
    | Empty, Empty -> true
    | Node (va, na), Node (vb, nb) -> va = vb && spec_node_eq_alt na nb
    | _, _ -> false
[@@verocaml.spec]
[@@verocaml.revealed]

let rec node_eq_refl (a : int node [@finite]) =
  [%verocaml.ensures fun _ -> spec_node_eq a a];
  [%verocaml.decreases a];
  match a with
    | Empty -> ()
    | Node (_, rest) -> node_eq_refl rest
[@@verocaml.proof]

let rec lemma_node_eq_implies_spec_node_eq (a : int node [@finite]) (b : int node [@finite]) =
  [%verocaml.requires a = b];
  [%verocaml.ensures fun _ -> spec_node_eq a b];
  [%verocaml.decreases a];
  match a with
    | Empty -> assert (b = a)
    | Node (av, ar) -> match b with
      | Empty -> assert false
      | Node (bv, br) ->
        assert (av = bv);
        lemma_node_eq_implies_spec_node_eq ar br
[@@verocaml.proof]

let stack_wf (stack : stack) : bool =
  (spec_node_len stack.top) = stack.length
[@@verocaml.spec]

let finite_sanity (stack : stack [@finite]) : stack =
  [%verocaml.ensures fun result -> result = stack];
  stack

let finite_node_sanity (node : int node [@finite]) : int node =
  [%verocaml.ensures fun result -> result = node];
  node

let did_push_front (value : int) (old : stack) (cur : stack) : bool =
  let expected = (spec_push_front value old) in
  cur.length = expected.length && (spec_node_eq cur.top expected.top)
[@@verocaml.spec]

let lemma_stack_push_len (value : int) (nodes : int node [@finite]) =
  [%verocaml.ensures fun _ ->
    let pushed = Node (value, nodes) in
    (spec_node_len pushed) = (spec_node_len nodes) + 1
  ];
  ()
[@@verocaml.proof]

let push_front (value : int) (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  [%verocaml.ensures fun result ->
    let { top; length } = stack in
    result = { top = Node (value, top); length = length + 1 }];
  [%verocaml.ensures fun result -> did_push_front value stack result];
  [%verocaml.ensures fun result -> stack_wf result];

  let { top; length; } = stack in
  let result = { top = Node (value, top); length = length + 1 } in

  [%verocaml.proof
    let pushed = spec_push_front value stack in
    node_eq_refl pushed.top;
    assert (result = spec_push_front value stack);
    lemma_node_eq_implies_spec_node_eq pushed.top result.top;
    assert (did_push_front value stack result);

    assert (pushed.length = (stack.length + 1));
    assert (stack.length = spec_node_len stack.top);
    assert ((spec_node_len (Node (value, top))) = spec_node_len result.top);

    lemma_stack_push_len value top;
    assert ((spec_node_len result.top) = ((spec_node_len stack.top) + 1));

    assert ((spec_node_len result.top) = result.length);
    assert (stack_wf result)
  ];

  result

let push_one_to_front (stack : stack [@finite]) : stack =
  [%verocaml.requires stack.length < 4611686018427387903];
  [%verocaml.requires stack_wf stack];
  push_front 1 stack

let lemma_empty_stack (stack : stack [@finite]) =
  [%verocaml.requires stack.length = 0 && (spec_is_empty stack.top)];
  [%verocaml.ensures fun result -> stack_wf stack];
  [%verocaml.reveal_with_fuel (spec_node_len, 2)];
  affirm ((spec_node_len Empty) = 0)
[@@verocaml.proof]

let empty_stack (_ : unit) : stack =
  [%verocaml.ensures fun result -> stack_wf result && result.length < 4611686018427387903];
  let empty = { top = Empty; length = 0; } in
  [%verocaml.proof
    lemma_empty_stack empty;
  ];
  empty

let inc x =
  [%verocaml.requires x < 100];
  [%verocaml.ensures fun result -> result = x + 1];
  x + 1

type 'a option =
  | None
  | Some of 'a

let spec_is_none (a : int option) : bool =
  match a with
    | None -> true
    | _ -> false
[@@verocaml.spec]

let spec_opt_eq (a : int option) (b : int option) : bool =
  match a with
    | None -> spec_is_none b
    | Some av -> (match b with
      | None -> false
      | Some bv -> av = bv)
[@@verocaml.spec]

let spec_node_value (node : int node) : int option =
  match node with
  | Empty -> None
  | Node (value, _) -> Some value
[@@verocaml.spec]

let rec spec_index_alt_imp (idx : int) (nodes : int node) : int node =
  [%verocaml.decreases nodes];
  match nodes with
    | Empty -> Empty
    | Node (value, rest) ->
      if idx <= 0 then Node (value, Empty)
      else spec_index_alt_imp (idx - 1) rest
[@@verocaml.spec]
[@@verocaml.revealed]

let spec_index (idx : int) (nodes : int node) : int option =
  match spec_index_alt_imp idx nodes with
    | Empty -> None
    | Node (value, _) -> Some value
[@@verocaml.spec]

let rec index_imp (idx : int) (nodes : int node [@finite]) : int node =
  [%verocaml.requires idx >= 0];
  [%verocaml.ensures fun result -> result = spec_index_alt_imp idx nodes];
  [%verocaml.decreases nodes];
  match nodes with
    | Empty -> Empty
    | Node (value, rest) ->
      if idx = 0 then Node (value, Empty)
      else index_imp (idx - 1) rest

let index (idx : int) (nodes : int node [@finite]) : int option =
  [%verocaml.requires idx >= 0];
  [%verocaml.ensures fun result -> spec_opt_eq result (spec_index idx nodes)];
  match index_imp idx nodes with
    | Empty -> None
    | Node (value, _) -> Some value

let rec lemma_node_len_positive (nodes : int node [@finite]) =
  [%verocaml.ensures fun _ -> (spec_node_len nodes) >= 0];
  [%verocaml.decreases nodes];

  match nodes with
    | Empty -> ()
    | Node (value, rest) ->
      lemma_node_len_positive rest;
      lemma_stack_push_len value rest;
      ()
[@@verocaml.proof]

let lemma_non_empty_len (nodes : int node [@finite]) =
  [%verocaml.requires not (spec_is_empty nodes)];
  [%verocaml.ensures fun _ -> (spec_node_len nodes) > 0];

  match nodes with
    | Empty -> impossible true
    | Node (value, rest) ->
        lemma_node_len_positive rest;
        lemma_stack_push_len value rest
[@@verocaml.proof]

let lemma_index_empty (idx : int) (nodes : int node [@finite]) =
  [%verocaml.requires spec_is_empty nodes];
  [%verocaml.ensures fun _ -> spec_is_empty (spec_index_alt_imp idx nodes)];
  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 2)];
  match nodes with
    | Empty -> assert ((spec_index_alt_imp idx nodes) = Empty)
    | Node _ -> impossible true
[@@verocaml.proof]

let lemma_index_node_step
    (idx : int)
    (value : int)
    (rest : int node [@finite]) =
  [%verocaml.requires idx > 0];
  [%verocaml.ensures fun _ ->
    spec_opt_eq
      (spec_index idx (Node (value, rest)))
      (spec_index (idx - 1) rest)];

  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 1)]
[@@verocaml.proof]

let lemma_opt_eq_none_trans
    (a : int option)
    (b : int option) =
  [%verocaml.requires spec_opt_eq a b];
  [%verocaml.requires spec_opt_eq b None];
  [%verocaml.ensures fun _ -> spec_opt_eq a None];

  match a with
  | None ->
      ()

  | Some av ->
      match b with
      | None -> assert false
      | Some bv -> assert false
[@@verocaml.proof]

let lemma_index_node_step_none
    (idx : int)
    (value : int)
    (rest : int node [@finite]) =
  [%verocaml.requires idx > 0];
  [%verocaml.requires
    spec_opt_eq (spec_index (idx - 1) rest) None];
  [%verocaml.ensures fun _ ->
    spec_opt_eq
      (spec_index idx (Node (value, rest)))
      None];

  [%verocaml.reveal_with_fuel (spec_index_alt_imp, 1)];

  let tail_result = spec_index_alt_imp (idx - 1) rest in

  match tail_result with
  | Empty ->
    ()
  | Node (tail_value, tail_rest) ->
      assert (spec_opt_eq (spec_index (idx - 1) rest) (Some tail_value));
      assert (spec_opt_eq (Some tail_value) None);
      assert false
[@@verocaml.proof]

let lemma_node_len_step
    (value : int)
    (rest : int node [@finite]) =
  [%verocaml.ensures fun _ ->
    spec_node_len (Node (value, rest))
      = 1 + spec_node_len rest];

  [%verocaml.reveal_with_fuel (spec_node_len, 1)]
[@@verocaml.proof]

let lemma_is_none_to_opt_eq_none (a : int option) =
  [%verocaml.requires spec_is_none a];
  [%verocaml.ensures fun _ -> spec_opt_eq a None];

  match a with
  | None -> ()
  | Some _ -> assert false
[@@verocaml.proof]

let lemma_opt_eq_none_to_is_none (a : int option) =
  [%verocaml.requires spec_opt_eq a None];
  [%verocaml.ensures fun _ -> spec_is_none a];

  match a with
  | None -> ()
  | Some _ -> assert false
[@@verocaml.proof]

let lemma_peel_push (nodes : int node) =
  [%verocaml.ensures fun _ -> (match nodes with
    | Empty -> true
    | Node (value, rest) -> Node (value, rest) = nodes)];
  ()
[@@verocaml.proof]

let rec lemma_index_oob (idx : int) (nodes : int node [@finite]) =
  [%verocaml.requires idx >= 0];
  [%verocaml.ensures fun _ -> implies (idx >= spec_node_len nodes) (spec_is_none (spec_index idx nodes))];
  [%verocaml.decreases nodes];
  match nodes with
    | Empty ->
      lemma_empty_len nodes;
      lemma_index_empty idx nodes;
    | Node (value, rest) ->
      if idx = 0 then begin
        lemma_non_empty_len nodes;
        assert (idx < (spec_node_len nodes));
      end else begin
        lemma_stack_push_len value rest;
        lemma_peel_push nodes;

        if idx >= spec_node_len nodes then begin
          lemma_index_oob (idx - 1) rest;
          lemma_is_none_to_opt_eq_none (spec_index (idx - 1) rest);
          lemma_opt_eq_none_to_is_none (spec_index idx nodes);

          assert (spec_is_none (spec_index idx nodes));
        end else
          assert (not (idx >= spec_node_len nodes));
      end
[@@verocaml.proof]

let rec push_n_nodes_spec (value : int) (count : int) (nodes : int node) : int node =
   [%verocaml.decreases count];
  if count <= 0 then nodes
  else
    push_n_nodes_spec
      value
      (count - 1)
      (Node (value, nodes))
[@@verocaml.spec]
[@@verocaml.revealed]

let rec push_n_nodes (value : int) (count : int) (nodes : int node [@finite]) : int node =
  [%verocaml.requires count >= 0];
  [%verocaml.ensures fun pushed ->
    ((spec_node_len pushed) = (spec_node_len nodes) + count)
  ];
  [%verocaml.decreases count];
  [%verocaml.proof
    lemma_stack_push_len value nodes
  ];

  if count = 0 then nodes
  else
    push_n_nodes
      value
      (count - 1)
      (Node (value, nodes))

let rec make_n_nodes (value : int) (count : int) : int node =
  [%verocaml.requires count >= 0];
  [%verocaml.ensures fun result -> (spec_node_len result) = count];
  [%verocaml.decreases count];
  if count = 0 then Empty
  else Node (value, (make_n_nodes value (count - 1)))
*)
