open Vstd

type int_list = Nil | Cons of int * int_list

let rec sum (xs : int_list) : Int.t =
  [%verocaml.decreases xs];
  match xs with Nil -> 0 | Cons (head, tail) -> head + sum tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec equal (left : int_list) (right : int_list) : bool =
  [%verocaml.decreases left];
  match left with
  | Nil -> (match right with Nil -> true | Cons _ -> false)
  | Cons (left_head, left_tail) ->
      (match right with
      | Nil -> false
      | Cons (right_head, right_tail) ->
          left_head = right_head && equal left_tail right_tail)
[@@verocaml.spec] [@@verocaml.revealed]

let rec equal_reflexive (xs : int_list [@finite]) : unit =
  [%verocaml.ensures fun _result -> equal xs xs];
  [%verocaml.decreases xs];
  match xs with
  | Nil -> ()
  | Cons (_, tail) ->
      equal_reflexive tail;
      ()
[@@verocaml.proof]

let rec equal_sum (left : int_list [@finite])
    (right : int_list [@finite]) : unit =
  [%verocaml.ensures fun _result ->
    not (equal left right) || sum left = sum right];
  [%verocaml.decreases left];
  match left with
  | Nil -> ()
  | Cons (_, left_tail) ->
      (match right with
      | Nil ->
          equal_sum left_tail Nil;
          ()
      | Cons (_, right_tail) ->
          equal_sum left_tail right_tail;
          ())
[@@verocaml.proof]
