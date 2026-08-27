let rec recurse (x : int) (y : int) : int =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) (y - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let opaque_public_symbol_is_total (x : int) (y : int) : unit =
  [%verocaml.ensures fun _result -> recurse x y = recurse x y];
  ()
[@@verocaml.proof]
