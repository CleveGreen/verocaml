let rec recurse (x : int) (y : int) : int =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) (y - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let reveal_on_one_path (take_path : bool) : unit =
  if take_path then (
    [%verocaml.reveal_with_fuel (recurse, 2)];
    ())
  else ()
[@@verocaml.proof]

let fresh_proof_scope (_n : int) : unit =
  ()
[@@verocaml.proof]

let reveal_in_let (take_left : bool) : unit =
  let reveal_token =
    if take_left then
      [%verocaml.reveal_with_fuel (recurse, 2)]
    else
      [%verocaml.reveal_with_fuel (recurse, 3)]
  in
  let _keep_named_unit_binding = reveal_token in
  [%verocaml.reveal_with_fuel (recurse, 4)]
[@@verocaml.proof]

let nested_scope (outer : bool) (inner : bool) : unit =
  if outer then (
    if inner then [%verocaml.reveal_with_fuel (recurse, 5)];
    [%verocaml.reveal_with_fuel (recurse, 6)]);
  [%verocaml.reveal_with_fuel (recurse, 7)]
[@@verocaml.proof]

let revealing_callee (_n : int) : unit =
  [%verocaml.reveal_with_fuel (recurse, 8)]
[@@verocaml.proof]

let caller_scope (n : int) : unit =
  revealing_callee n
[@@verocaml.proof]

let final_fresh_scope (_n : int) : unit =
  ()
[@@verocaml.proof]
