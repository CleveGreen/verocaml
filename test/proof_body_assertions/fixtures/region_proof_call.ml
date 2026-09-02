type int_list =
  | Nil
  | Cons of int * int_list

let rec list_len (values : int_list) : Vstd.Int.t =
  [%verocaml.decreases values];
  match values with Nil -> 0 | Cons (_, tail) -> 1 + list_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec twice_len (values : int_list) : Vstd.Int.t =
  [%verocaml.decreases values];
  match values with Nil -> 0 | Cons (_, tail) -> 2 + twice_len tail
[@@verocaml.spec] [@@verocaml.revealed]

let rec prove_twice_len (values : int_list [@finite]) =
  [%verocaml.ensures fun _result ->
    twice_len values = list_len values + list_len values];
  [%verocaml.decreases values];
  match values with
  | Nil -> ()
  | Cons (_, tail) -> prove_twice_len tail
[@@verocaml.proof]

let region_proof_call (values : int_list [@finite]) =
  [%verocaml.proof
    (match values with
     | Nil -> ()
     | Cons (_, tail) -> prove_twice_len tail);
    prove_twice_len values;
    [%verocaml.assert true];
    ()];
  [%verocaml.proof
    (match values with Nil -> () | Cons _ -> ());
    [%verocaml.assert
      twice_len values = list_len values + list_len values];
    ()];
  ()
