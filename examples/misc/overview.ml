[@@@verocaml.verify]

let implies premise conclusion = (not premise) || conclusion
[@@verocaml.spec]

let iff a b = a = b
[@@verocaml.spec]

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

type 'a node =
  | Empty
  | Node of 'a * 'a node

let spec_is_empty a =
  match a with
    | Empty  -> true
    | Node _ -> false
[@@verocaml.spec]

let rec spec_node_len (node : 'a node) =
  [%verocaml.decreases node];
  match node with
    | Empty -> 0
    | Node (_, next) -> 1 + spec_node_len next
[@@verocaml.spec]
[@@verocaml.revealed]

let rec lemma_node_len_positive_i (n : 'a node [@finite]) =
  [%verocaml.ensures fun _ -> (spec_node_len n) >= 0];
  [%verocaml.decreases n];
  match n with
    | Empty -> ()
    | Node (_, next) -> lemma_node_len_positive_i next
[@@verocaml.proof]

let lemma_node_len_positive (n : 'a node [@finite]) =
  [%verocaml.ensures fun _ -> (spec_node_len n) [@trigger] >= 0];
  lemma_node_len_positive_i n
[@@verocaml.proof]
[@@verocaml.broadcast]

let lemma_non_empty_node_len (n : 'a node [@finite]) =
  [%verocaml.requires not (spec_is_empty n)];
  [%verocaml.ensures fun _ -> (spec_node_len n) [@trigger] > 0];
  match n with
    | Empty -> assert false
    | Node (value, rest) ->
      lemma_node_len_positive rest
[@@verocaml.proof]

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

let opt_is_some (o : 'a option) =
  match o with
    | Some _ -> true
    | None -> false
[@@verocaml.spec]

let rec spec_index (i : Vstd.Int.t) (n : 'a node) : 'a option =
  [%verocaml.decreases n];
  match n with
    | Empty -> None
    | Node (value, next) ->
      if i <= 0 then (Some value)
      else spec_index (i - 1) next
[@@verocaml.spec]
[@@verocaml.revealed]

let rec lemma_index_oob (i : Vstd.Int.t) (n : 'a node [@finite]) : unit =
  [%verocaml.requires i >= 0];
  [%verocaml.ensures fun _ -> iff (i >= spec_node_len n) ((spec_index i n) = None)];
  [%verocaml.decreases n];
  match n with
    | Empty -> ()
    | Node (value, rest) ->
      if i = 0 then begin
        lemma_non_empty_node_len n;
        assert (opt_is_some (spec_index i n));
      end else begin
        lemma_index_oob (i - 1) rest;
        assert (
          iff
            (i >= 1 + spec_node_len rest)
            ((i - 1) >= spec_node_len rest)
        )
      end
[@@verocaml.proof]

[%%verocaml.symbolic val unbound_value : 'a]

let spec_unwrap (o : 'a option) : 'a =
  match o with
    | None -> (unbound_value : 'a)
    | Some v -> v
[@@verocaml.spec]

let assume_index (i : Vstd.Int.t) (n : 'a node) : 'a =
  spec_unwrap (spec_index i n)
[@@verocaml.spec]

let lemma_assume_index (i : Vstd.Int.t) (n : 'a node [@finite]) =
  [%verocaml.requires 0 <= i && i < spec_node_len n];
  [%verocaml.ensures fun _ -> (match spec_index i n with
    | None -> false
    | Some value -> value = (assume_index i n))
  ];
  lemma_index_oob i n;
  let si = spec_index i n in
  assert (opt_is_some si);
  ()
[@@verocaml.proof]

let seq_indexer (n : 'a node) : Vstd.Int.t -> 'a =
  fun i -> assume_index i n
[@@verocaml.spec]

let seq_view (n : 'a node) : 'a Vstd.Seq.t =
  Seq.init (spec_node_len n) (seq_indexer n)
[@@verocaml.spec]

let lemma_index_view (i : Vstd.Int.t) (n : 'a node [@finite]) =
  [%verocaml.requires 0 <= i && i < spec_node_len n];
  [%verocaml.ensures fun _ -> Vstd.Seq.get (seq_view n) i = assume_index i n];
  Vstd.Seq.axiom_init_get
    (spec_node_len n)
    (seq_indexer n)
    i
[@@verocaml.proof]
