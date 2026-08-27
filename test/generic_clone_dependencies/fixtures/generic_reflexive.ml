type 'a seq = Nil | Cons of 'a * 'a seq

let rec equal (left : 'a seq) (right : 'a seq) : bool =
  [%verocaml.decreases left];
  match left with
  | Nil -> (match right with Nil -> true | Cons _ -> false)
  | Cons (x, xs) ->
      (match right with
      | Nil -> false
      | Cons (y, ys) -> x = y && equal xs ys)
[@@verocaml.spec] [@@verocaml.revealed]

let rec reflexive (xs : 'a seq [@finite]) : unit =
  [%verocaml.ensures fun _result -> equal xs xs];
  [%verocaml.decreases xs];
  match xs with
  | Nil -> ()
  | Cons (_, tail) ->
      reflexive tail;
      ()
[@@verocaml.proof]

let int_equal_control (xs : int seq) : bool = equal xs xs
[@@verocaml.spec]

let bool_equal_control (xs : bool seq) : bool = equal xs xs
[@@verocaml.spec]

let int_reflexive_control (xs : int seq [@finite]) : unit = reflexive xs
[@@verocaml.proof]

let bool_reflexive_control (xs : bool seq [@finite]) : unit = reflexive xs
[@@verocaml.proof]
