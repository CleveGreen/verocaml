let specified (x : int) : int =
  [%verocaml.requires x < 1000];
  [%verocaml.ensures fun result -> result = (x + 1)];
  x + 1

(* This should be forbidden, something already with a specification
   may not be re-specified *)
let overwrite_spec (x : int) : int =
  [%verocaml.ensures fun result -> result = x];
  specified x
[@@verocaml.external_specification]

let affirm (c : bool) : unit =
  [%verocaml.requires c];
  ()
[@@verocaml.proof]

let bad (x : int) : unit =
  (* x loses its precondition, meaning it could overflow... *)
  let next = specified x in
  [%verocaml.proof
    (* but next is x + 1, so this is false, and should fail as well *)
    affirm (next = x)
  ];
  ()
