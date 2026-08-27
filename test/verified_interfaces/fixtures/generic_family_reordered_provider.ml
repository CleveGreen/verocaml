type 'a snapshot = End | More of 'a * 'a snapshot


let rec equal (left : 'a snapshot) (right : 'a snapshot) : bool =
  [%verocaml.decreases left];
  match left with
  | End -> (match right with End -> true | More _ -> false)
  | More (x, xs) ->
      (match right with
      | End -> false
      | More (y, ys) -> x = y && equal xs ys)
[@@verocaml.spec] [@@verocaml.revealed]

let rec reflexive (xs : 'a snapshot [@finite]) : unit =
  [%verocaml.ensures fun _result -> equal xs xs];
  [%verocaml.decreases xs];
  match xs with
  | End -> ()
  | More (_, tail) ->
      reflexive tail;
      ()
[@@verocaml.proof]

let rec symmetric
    (left : 'a snapshot [@finite])
    (right : 'a snapshot [@finite]) : unit =
  [%verocaml.ensures
    fun _result -> (not (equal left right)) || equal right left];
  [%verocaml.decreases left];
  match left with
  | End -> ()
  | More (x, xs) ->
      (match right with
      | End -> ()
      | More (y, ys) ->
          if x = y then symmetric xs ys else ())
[@@verocaml.proof]

let equal_self (xs : 'a snapshot) : bool = equal xs xs
[@@verocaml.spec]

let equal_self_again (xs : 'a snapshot) : bool = equal_self xs
[@@verocaml.spec]

let reflexive_wrapper (xs : 'a snapshot [@finite]) : unit =
  [%verocaml.ensures fun _result -> equal xs xs];
  reflexive xs
[@@verocaml.proof]

let int_equal_control (xs : int snapshot) : bool = equal xs xs
[@@verocaml.spec]

let bool_equal_control (xs : bool snapshot) : bool = equal xs xs
[@@verocaml.spec]

let int_reflexive_control (xs : int snapshot [@finite]) : unit = reflexive xs
[@@verocaml.proof]

let bool_reflexive_control (xs : bool snapshot [@finite]) : unit = reflexive xs
[@@verocaml.proof]

let int_symmetric_control
    (xs : int snapshot [@finite])
    (ys : int snapshot [@finite]) : unit =
  symmetric xs ys
[@@verocaml.proof]

let bool_symmetric_control
    (xs : bool snapshot [@finite])
    (ys : bool snapshot [@finite]) : unit =
  symmetric xs ys
[@@verocaml.proof]

let int_equal_self_control (xs : int snapshot) : bool = equal_self xs
[@@verocaml.spec]

let bool_equal_self_control (xs : bool snapshot) : bool = equal_self xs
[@@verocaml.spec]

let int_equal_self_again_control (xs : int snapshot) : bool =
  equal_self_again xs
[@@verocaml.spec]

let bool_equal_self_again_control (xs : bool snapshot) : bool =
  equal_self_again xs
[@@verocaml.spec]

let int_wrapper_control (xs : int snapshot [@finite]) : unit =
  reflexive_wrapper xs
[@@verocaml.proof]

let bool_wrapper_control (xs : bool snapshot [@finite]) : unit =
  reflexive_wrapper xs
[@@verocaml.proof]


module type STACK = sig
  type t

  val model : t @ read -> int snapshot @ immutable
  val invariant : t @ read -> bool
  val established : int -> t @ unique
  val nested_successor : int -> int -> t @ unique
  val reroot_successor : int -> int -> t @ unique
  val refresh : t @ unique -> t @ unique
  val length : t @ read -> int
end

module Stack : STACK = struct
  type node =
    | Empty
    | Node of { mutable value : int; mutable next : node }

  type t = { mutable top : node; mutable length : int }

  let rec contents_node (node : node) : int snapshot =
    [%verocaml.decreases node];
    match node with
    | Empty -> End
    | Node { value; next } -> More (value, contents_node next)
  [@@verocaml.spec]
  [@@verocaml.opaque]

  let model (stack : t @ read) : int snapshot @ immutable =
    contents_node stack.top
  [@@verocaml.spec]

  let invariant (stack : t @ read) =
    let contents = model stack in
    contents = contents
  [@@verocaml.type_invariant]

  let established value : t @ unique =
    { top = Node { value; next = Empty }; length = 1 }

  let nested_successor first second : t @ unique =
    [%verocaml.ensures fun result -> model result = More (first, End)];
    let stack =
      {
        top =
          Node
            {
              value = first;
              next = Node { value = second; next = Empty };
            };
        length = 2;
      }
    in
    (match stack.top with
    | Empty -> ()
    | Node record -> record.next <- Empty);
    stack

  let reroot_successor first second : t @ unique =
    [%verocaml.ensures fun result -> model result = More (second, End)];
    let stack =
      {
        top =
          Node
            {
              value = first;
              next = Node { value = second; next = Empty };
            };
        length = 2;
      }
    in
    (match stack.top with
    | Empty -> ()
    | Node { value = _; next } -> stack.top <- next);
    stack

  let refresh (stack : t @ unique) : t @ unique =
    if false then
      match stack.top with
      | Empty -> stack
      | Node record ->
          record.value <- 0;
          stack
    else stack

  let length (stack : t @ read) = stack.length
end

let run value =
  let stack = Stack.established value in
  Stack.refresh stack
