[@@@verocaml.verify]

type 'a vlist = Nil | Cons of 'a * 'a vlist

let rec equal (left : 'a vlist) (right : 'a vlist) : bool =
  [%verocaml.decreases left];
  match left with
  | Nil -> (match right with Nil -> true | Cons _ -> false)
  | Cons (head, tail) ->
      (match right with
      | Nil -> false
      | Cons (other, rest) -> head = other && equal tail rest)
[@@verocaml.spec]
[@@verocaml.revealed]

let rec list_length (value : 'a vlist) : Vstd.Int.t =
  [%verocaml.decreases value];
  match value with Nil -> 0 | Cons (_, tail) -> 1 + list_length tail
[@@verocaml.spec]
[@@verocaml.revealed]

let rec equal_reflexive (value : 'a vlist [@finite]) : unit =
  [%verocaml.ensures fun _result -> equal value value];
  [%verocaml.decreases value];
  [%verocaml.reveal_with_fuel (equal, 2)];
  match value with
  | Nil -> ()
  | Cons (_, tail) ->
      equal_reflexive tail;
      ()
[@@verocaml.proof]

let rec equal_sound
    (left : 'a vlist [@finite])
    (right : 'a vlist [@finite]) : unit =
  [%verocaml.ensures
    fun _result -> (not (equal left right)) || left = right];
  [%verocaml.decreases left];
  [%verocaml.reveal_with_fuel (equal, 2)];
  match left with
  | Nil -> ()
  | Cons (head, tail) ->
      (match right with
      | Nil -> ()
      | Cons (other, rest) ->
          if head = other then equal_sound tail rest else ())
[@@verocaml.proof]

let int_equal_control (left : int vlist) (right : int vlist) =
  equal left right
[@@verocaml.spec]

let bool_equal_control (left : bool vlist) (right : bool vlist) =
  equal left right
[@@verocaml.spec]

let int_length_control (value : int vlist) = list_length value
[@@verocaml.spec]

let bool_length_control (value : bool vlist) = list_length value
[@@verocaml.spec]

let int_reflexive (value : int vlist [@finite]) = equal_reflexive value
[@@verocaml.proof]

let bool_reflexive (value : bool vlist [@finite]) = equal_reflexive value
[@@verocaml.proof]

let int_sound
    (left : int vlist [@finite])
    (right : int vlist [@finite]) =
  equal_sound left right
[@@verocaml.proof]

let bool_sound
    (left : bool vlist [@finite])
    (right : bool vlist [@finite]) =
  equal_sound left right
[@@verocaml.proof]

let stack_wf (contents : int vlist) (length : Vstd.Int.t) : bool =
  int_length_control contents = length
[@@verocaml.spec]

let spec_push_front (value : int) (contents : int vlist) : int vlist =
  Cons (value, contents)
[@@verocaml.spec]

let push_front_preserves_length
    (value : int)
    (contents : int vlist [@finite])
    (length : Vstd.Int.t) : unit =
  [%verocaml.requires stack_wf contents length];
  [%verocaml.ensures
    fun _result ->
      stack_wf (spec_push_front value contents) (length + 1)];
  [%verocaml.reveal_with_fuel (list_length, 1)]
[@@verocaml.proof]

module type STACK = sig
  type t

  val model : t @ read -> int vlist @ immutable
  val invariant : t @ read -> bool
  val create : unit -> t @ unique
  val singleton : int -> t @ unique
  val push : int -> int -> t @ unique
  val pop : int -> int -> t @ unique
  val drop : t @ unique -> t @ unique
  val length : t @ read -> int
  val is_empty : t @ read -> bool
  val top : t @ read -> int
  val peek : t @ read -> int
end

module Stack : STACK = struct
  type node =
    | Empty
    | Node of { mutable value : int; mutable next : node }

  type t = {
    mutable top : node;
    mutable length : int;
    mutable head : int;
  }

  let rec contents_node (node : node) : int vlist =
    [%verocaml.decreases node];
    match node with
    | Empty -> Nil
    | Node { value; next } -> Cons (value, contents_node next)
  [@@verocaml.spec]
  [@@verocaml.opaque]

  let model (stack : t @ read) : int vlist @ immutable =
    contents_node stack.top
  [@@verocaml.spec]

  let invariant (stack : t @ read) =
    let contents = model stack in
    contents = contents
  [@@verocaml.type_invariant]

  let create () : t @ unique = { top = Empty; length = 0; head = 0 }

  let singleton value : t @ unique =
    { top = Node { value; next = Empty }; length = 1; head = value }

  let push first second : t @ unique =
    [%verocaml.ensures fun result ->
      model result =
      spec_push_front first (spec_push_front second Nil)];
    let stack =
      {
        top = Node { value = second; next = Empty };
        length = 2;
        head = first;
      }
    in
    [%verocaml.proof
      [%verocaml.assert model stack = Cons (second, Nil)];
      ()];
    stack.top <- Node { value = first; next = stack.top };
    stack

  let pop first second : t @ unique =
    [%verocaml.ensures fun result -> model result = Cons (second, Nil)];
    let stack =
      {
        top =
          Node
            {
              value = first;
              next = Node { value = second; next = Empty };
            };
        length = 1;
        head = second;
      }
    in
    (match stack.top with
    | Empty -> ()
    | Node { value = _; next } -> stack.top <- next);
    stack

  let drop (stack : t @ unique) : t @ unique =
    if false then
      match stack.top with
      | Empty -> stack
      | Node record ->
          record.value <- 0;
          stack
    else stack

  let length (stack : t @ read) = stack.length
  let is_empty (stack : t @ read) = stack.length = 0
  let top (stack : t @ read) = stack.head
  let peek (stack : t @ read) = stack.head
end

let nonnegative (value : int) : bool = value >= 0 [@@verocaml.spec]

let tracked_successor
    (value : Vstd.Int.t [@tracked]) : (Vstd.Int.t [@tracked]) =
  [%verocaml.requires value >= 0];
  [%verocaml.ensures fun result -> result = value + 1];
  let[@tracked] next = (((value [@tracked]) + 1) [@tracked]) in
  (next [@tracked])
[@@verocaml.proof]

let mode_observation (source : int) =
  [%verocaml.requires nonnegative source];
  [%verocaml.ensures fun result -> result = source];
  let[@tracked] tracked = (source [@tracked]) in
  let[@ghost] ghost = (source [@ghost]) in
  [%verocaml.proof
    let _ = nonnegative source in
    let _ = nonnegative tracked in
    let _ = nonnegative ghost in
    let[@tracked] next =
      (tracked_successor (tracked [@tracked]) [@tracked])
    in
    let _ = (next [@tracked]) in
    ()];
  source

let list_controls () =
  [%verocaml.proof
    int_reflexive (Cons (1, Cons (2, Nil)));
    bool_reflexive (Cons (true, Cons (false, Nil)));
    int_sound (Cons (1, Nil)) (Cons (1, Nil));
    bool_sound (Cons (true, Nil)) (Cons (true, Nil));
    push_front_preserves_length 3 (Cons (1, Cons (2, Nil))) 2];
  ()

let drop_control value =
  let stack = Stack.singleton value in
  Stack.drop stack
