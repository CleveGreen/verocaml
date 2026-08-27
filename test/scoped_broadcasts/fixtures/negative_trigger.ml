let observed (_value : int) : bool = true [@@verocaml.spec]

let missing_trigger (value : int) : unit =
  [%verocaml.ensures fun _ -> observed value || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]
