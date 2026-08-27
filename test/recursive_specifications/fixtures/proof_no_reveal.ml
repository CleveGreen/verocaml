let rec recurse (x : int) (y : int) : int =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) (y - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let opaque_equation_is_unavailable (x : int) : unit =
  [%verocaml.ensures fun _result -> recurse x 1 = x + 1];
  ()
[@@verocaml.proof]
