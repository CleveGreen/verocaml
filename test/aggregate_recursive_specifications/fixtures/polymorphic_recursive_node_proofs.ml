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
  length : Vstd.Int.t;
}

let spec_is_empty a =
  match a with
    | Empty -> true
    | Node _ -> false
[@@verocaml.spec]

let rec spec_node_len (node : 'a node) : Vstd.Int.t =
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

let spec_push_front (value : 'a) (stack : 'a stack) : 'a stack =
  let { top; length; } = stack in
  { top = Node (value, top); length = length + 1 }
[@@verocaml.spec]

let rec spec_node_eq (a : 'a node) (b : 'a node) : bool =
  [%verocaml.decreases a];
  match (a, b) with
    | Empty, Empty -> true
    | Node (va, na), Node(vb, nb) -> va = vb && spec_node_eq na nb
    | _, _ -> false
[@@verocaml.spec]
[@@verocaml.revealed]

let rec node_eq_symm (a : 'a node [@finite]) (b : 'a node [@finite]) =
  [%verocaml.requires spec_node_eq a b];
  [%verocaml.ensures fun _ -> spec_node_eq b a];
  [%verocaml.decreases a];
  match a, b with
    | Node (va, na), Node(vb, nb) -> node_eq_symm na nb
    | _, _ -> ()
[@@verocaml.proof]

let rec node_eq_trans (a : 'a node [@finite]) (b : 'a node [@finite]) (c : 'a node [@finite]) =
  [%verocaml.requires spec_node_eq a b];
  [%verocaml.requires spec_node_eq b c];
  [%verocaml.ensures fun _ -> spec_node_eq a c];
  [%verocaml.decreases a];
  match a, b, c with
    | Node (_, na), Node (_, nb), Node (_, nc) -> node_eq_trans na nb nc
    | _, _, _ -> ()
[@@verocaml.proof]

let rec node_eq_refl (a : 'a node [@finite]) =
  [%verocaml.ensures fun _ -> spec_node_eq a a];
  [%verocaml.decreases a];
  match a with
    | Empty -> ()
    | Node (_, rest) -> node_eq_refl rest
[@@verocaml.proof]

let prec_node (a : 'a node) : bool =
  match a with
    | Empty -> true
    | Node (v, na) -> Node (v, na) = a
[@@verocaml.spec]

let rec lemma_spec_eq (a : 'a node [@finite]) (b : 'a node [@finite]) =
  [%verocaml.requires spec_node_eq a b];
  [%verocaml.ensures fun _ -> a = b];
  [%verocaml.decreases a];
  match a, b with
    | Empty, Empty -> ()
    | Node (va, na), Node (vb, nb) -> lemma_spec_eq na nb
    | _, _ -> (* impossible *) ()
[@@verocaml.proof]

let lemma_spec_eq_iff (a : 'a node [@finite]) (b : 'a node [@finite]) =
  [%verocaml.ensures fun _ -> iff (spec_node_eq a b) (a = b)];
  if spec_node_eq a b then lemma_spec_eq a b
  else if a <> b then begin 
    if spec_node_eq a b then lemma_spec_eq a b;
    ()
  end else node_eq_refl a
[@@verocaml.proof]
