let rec recurse (x : int) (y : int) : int =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) (y - 1)
[@@verocaml.spec] [@@verocaml.revealed]

let reveal_on_one_path (take_path : bool) : unit =
  if take_path then (
    [%verocaml.reveal_with_fuel (recurse, 2)];
    ())
  else ()
[@@verocaml.proof]

let fresh_proof_scope (_n : int) : unit =
  ()
[@@verocaml.proof]
