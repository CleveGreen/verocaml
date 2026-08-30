type 'a option_specification = 'a option
[@@verocaml.external_type_specification]

type 'a seq = Nil | Cons of 'a * 'a seq
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let observed (_value : 'a) : bool = true [@@verocaml.spec]

let observed_pair (_left : int) (_right : bool) : bool =
  true
[@@verocaml.spec]

let observed_adt
    (_value : 'a)
    (_optional : 'a option)
    (_sequence : 'a seq) : bool =
  true
[@@verocaml.spec]

let observed_fold
    (_sequence : 'a seq)
    (_tree : 'a tree)
    (_optional : 'a option) : bool =
  true
[@@verocaml.spec]

let lemma_abstract (value : 'a) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger]) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

let lemma_scalar_pair (number : int) (flag : bool) : unit =
  [%verocaml.ensures fun _ ->
    ((observed_pair number flag) [@trigger])
    || (number = number && flag = flag)];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

let lemma_adt_vector
    (value : 'a)
    (optional : 'a option)
    (sequence : 'a seq) : unit =
  [%verocaml.ensures fun _ ->
    ((observed_adt value optional sequence) [@trigger])
    ||
    match optional with
    | None -> value = value
    | Some payload -> payload = payload && sequence = sequence];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

let lemma_map_fold_shape
    (sequence : 'a seq)
    (tree : 'a tree)
    (optional : 'a option) : unit =
  [%verocaml.ensures fun _ ->
    ((observed_fold sequence tree optional) [@trigger])
    ||
    match tree with
    | Leaf -> sequence = sequence
    | Node (value, left, right) ->
        value = value && left = left && right = right && optional = optional];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

[@@@verocaml.activate
  [lemma_abstract; lemma_scalar_pair; lemma_adt_vector; lemma_map_fold_shape]]

let use_abstract (value : 'a) : unit =
  [%verocaml.requires observed value];
  [%verocaml.requires
    forall (fun (candidate : 'a) ->
      ((observed candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.assert value = value];
  ()
[@@verocaml.proof]

let use_scalars (number : int) (flag : bool) : unit =
  [%verocaml.requires observed_pair number flag];
  [%verocaml.requires
    forall (fun (candidate : int) ->
      ((observed_pair candidate flag) [@trigger]) || candidate = candidate)];
  [%verocaml.assert number = number && flag = flag];
  ()
[@@verocaml.proof]

let use_option_seq_tree
    (value : int)
    (optional : int option)
    (sequence : int seq)
    (tree : int tree) : unit =
  [%verocaml.requires observed_adt value optional sequence];
  [%verocaml.requires observed_fold sequence tree optional];
  [%verocaml.requires
    forall (fun (candidate : int) ->
      ((observed_adt candidate optional sequence) [@trigger])
      || candidate = candidate)];
  [%verocaml.requires
    forall (fun (candidate : int seq) ->
      ((observed_fold candidate tree optional) [@trigger])
      || candidate = candidate)];
  [%verocaml.assert optional = optional && sequence = sequence && tree = tree];
  ()
[@@verocaml.proof]

let use_bool_option
    (value : bool)
    (optional : bool option)
    (sequence : bool seq) : unit =
  [%verocaml.requires observed_adt value optional sequence];
  [%verocaml.requires
    forall (fun (candidate : bool) ->
      ((observed_adt candidate optional sequence) [@trigger])
      || candidate = candidate)];
  [%verocaml.assert value = value && optional = optional];
  ()
[@@verocaml.proof]
