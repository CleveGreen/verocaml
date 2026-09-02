(** Immutable mathematical sequences for specifications and proofs.
 *)

type +'a t = private Sequence_token of Int.t

[%%verocaml.symbolic val empty : 'a t]
[%%verocaml.symbolic val init : Int.t -> (Int.t -> 'a) -> 'a t]
[%%verocaml.symbolic val length : 'a t -> Int.t]
[%%verocaml.symbolic val get : 'a t -> Int.t -> 'a]
[%%verocaml.symbolic val push : 'a t -> 'a -> 'a t]
[%%verocaml.symbolic val update : 'a t -> Int.t -> 'a -> 'a t]
[%%verocaml.symbolic val subrange : 'a t -> Int.t -> Int.t -> 'a t]
[%%verocaml.symbolic val append : 'a t -> 'a t -> 'a t]

let valid_length (size : Int.t) : bool =
  0 <= size
[@@verocaml.spec]

let valid_index (sequence : 'a t) (index : Int.t) : bool =
  0 <= index && index < length sequence
[@@verocaml.spec]

let valid_subrange (sequence : 'a t) (lower : Int.t) (upper : Int.t) : bool =
  0 <= lower && lower <= upper && upper <= length sequence
[@@verocaml.spec]

let is_empty (sequence : 'a t) : bool =
  length sequence = 0
[@@verocaml.spec]

let first (sequence : 'a t) : 'a =
  get sequence 0
[@@verocaml.spec]

let last (sequence : 'a t) : 'a =
  get sequence (length sequence - 1)
[@@verocaml.spec]

let singleton (value : 'a) : 'a t =
  push empty value
[@@verocaml.spec]

let take (sequence : 'a t) (count : Int.t) : 'a t =
  subrange sequence 0 count
[@@verocaml.spec]

let drop (sequence : 'a t) (count : Int.t) : 'a t =
  subrange sequence count (length sequence)
[@@verocaml.spec]

let skip (sequence : 'a t) (count : Int.t) : 'a t =
  drop sequence count
[@@verocaml.spec]

let ext_equal (left : 'a t) (right : 'a t) : bool =
  length left = length right
  && forall (fun (index : Int.t) ->
         (not (valid_index left index))
         || ((get left index) [@trigger]) = get right index)
[@@verocaml.spec]

let contains (sequence : 'a t) (value : 'a) : bool =
  exists (fun (index : Int.t) ->
      valid_index sequence index
      && get sequence index = value)
[@@verocaml.spec]

let axiom_length_domain (sequence : 'a t) : unit =
  [%verocaml.ensures
    fun _ ->
      valid_length (length sequence [@trigger])
      && length (empty : 'a t) = 0];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_init_length (size : Int.t) (constructor : Int.t -> 'a) : unit =
  [%verocaml.requires valid_length size];
  [%verocaml.ensures
    fun _ -> length (init size constructor) [@trigger] = size];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_init_get
    (size : Int.t)
    (constructor : Int.t -> 'a)
    (index : Int.t) : unit =
  [%verocaml.requires valid_length size];
  [%verocaml.requires 0 <= index && index < size];
  [%verocaml.ensures
    fun _ -> get (init size constructor) index [@trigger] = constructor index];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_push_length (sequence : 'a t) (value : 'a) : unit =
  [%verocaml.ensures
    fun _ -> length (push sequence value) [@trigger] = length sequence + 1];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_push_get_last (sequence : 'a t) (value : 'a) : unit =
  [%verocaml.ensures
    fun _ -> get (push sequence value) (length sequence) [@trigger] = value];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_push_get_old
    (sequence : 'a t)
    (value : 'a)
    (index : Int.t) : unit =
  [%verocaml.requires valid_index sequence index];
  [%verocaml.ensures
    fun _ -> get (push sequence value) index [@trigger] = get sequence index];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_update_length
    (sequence : 'a t)
    (index : Int.t)
    (value : 'a) : unit =
  [%verocaml.requires valid_index sequence index];
  [%verocaml.ensures
    fun _ -> length (update sequence index value) [@trigger] = length sequence];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_update_get_same
    (sequence : 'a t)
    (index : Int.t)
    (value : 'a) : unit =
  [%verocaml.requires valid_index sequence index];
  [%verocaml.ensures
    fun _ -> get (update sequence index value) index [@trigger] = value];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_update_get_other
    (sequence : 'a t)
    (updated_index : Int.t)
    (value : 'a)
    (observed_index : Int.t) : unit =
  [%verocaml.requires valid_index sequence updated_index];
  [%verocaml.requires valid_index sequence observed_index];
  [%verocaml.requires observed_index <> updated_index];
  [%verocaml.ensures
    fun _ ->
      get (update sequence updated_index value) observed_index [@trigger]
      = get sequence observed_index];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_subrange_length
    (sequence : 'a t)
    (lower : Int.t)
    (upper : Int.t) : unit =
  [%verocaml.requires valid_subrange sequence lower upper];
  [%verocaml.ensures
    fun _ -> length (subrange sequence lower upper) [@trigger] = upper - lower];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_subrange_get
    (sequence : 'a t)
    (lower : Int.t)
    (upper : Int.t)
    (index : Int.t) : unit =
  [%verocaml.requires valid_subrange sequence lower upper];
  [%verocaml.requires 0 <= index && index < upper - lower];
  [%verocaml.ensures
    fun _ ->
      get (subrange sequence lower upper) index [@trigger]
      = get sequence (lower + index)];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_append_length (left : 'a t) (right : 'a t) : unit =
  [%verocaml.ensures
    fun _ -> length (append left right) [@trigger] = length left + length right];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_append_get_left
    (left : 'a t)
    (right : 'a t)
    (index : Int.t) : unit =
  [%verocaml.requires valid_index left index];
  [%verocaml.ensures
    fun _ -> get (append left right) index [@trigger] = get left index];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_append_get_right
    (left : 'a t)
    (right : 'a t)
    (index : Int.t) : unit =
  [%verocaml.requires length left <= index];
  [%verocaml.requires index < length left + length right];
  [%verocaml.ensures
    fun _ ->
      get (append left right) index [@trigger]
      = get right (index - length left)];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

let axiom_extensionality (left : 'a t) (right : 'a t) : unit =
  [%verocaml.requires ext_equal left right [@trigger]];
  [%verocaml.ensures fun _ -> left = right];
  ()
[@@verocaml.axiom]
[@@verocaml.broadcast]

[@@@verocaml.broadcast_group (group_seq_axioms, [
  axiom_length_domain;
  axiom_init_length;
  axiom_init_get;
  axiom_push_length;
  axiom_push_get_last;
  axiom_push_get_old;
  axiom_update_length;
  axiom_update_get_same;
  axiom_update_get_other;
  axiom_subrange_length;
  axiom_subrange_get;
  axiom_append_length;
  axiom_append_get_left;
  axiom_append_get_right
])]
