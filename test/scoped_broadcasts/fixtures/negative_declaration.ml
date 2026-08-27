let observed (_value : int) : bool = true [@@verocaml.spec]

let invalid_axiom (value : int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])];
  ()
[@@verocaml.external_body]
[@@verocaml.broadcast]
