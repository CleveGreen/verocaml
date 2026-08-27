let observed_function (_value : int -> int) : bool = true [@@verocaml.spec]

let function_binder (value : int -> int) : unit =
  [%verocaml.ensures fun _ -> ((observed_function value) [@trigger])];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]
