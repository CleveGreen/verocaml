let rec recurse (x : Vstd.Int.t) (y : Vstd.Int.t) : Vstd.Int.t =
  [%verocaml.decreases y];
  if y <= 0 then x else recurse (x + 1) (y - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let opaque_equation_is_unavailable (x : Vstd.Int.t) : unit =
  [%verocaml.ensures fun _result -> recurse x 1 = x + 1];
  ()
[@@verocaml.proof]
