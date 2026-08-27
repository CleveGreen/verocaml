type 'a seq = Nil | Cons of 'a * 'a seq

let observe (xs : 'a seq) : unit =
  Sys.opaque_identity xs;
  ()
[@@verocaml.proof]

let int_control (xs : int seq) : unit = observe xs
[@@verocaml.proof]

let bool_control (xs : bool seq) : unit = observe xs
[@@verocaml.proof]
