let helper (x : int) = x + 1

let rec countdown (n : int) =
  if n <= 0 then 0 else countdown (n - 1)

let pure (x : int) =
  let y = x + 1 in
  let pair = (y, x) in
  if y > x then
    match pair with
    | a, b when a > b -> a - b
    | _ -> helper (-x)
  else
    x * 3

let booleans (a : int) (b : int) =
  not false && (a = b || true)

let total_case : bool -> int = function
  | true -> 1
  | false -> 0

let contracted (n : int) =
  [%verocaml.requires n >= 0];
  [%verocaml.ensures fun result -> result >= [%verocaml.old n]];
  [%verocaml.decreases n];
  [%verocaml.assert n >= 0];
  n

let unit_value () = ()

let standard_arithmetic (x : int) = succ x + pred (abs x)
