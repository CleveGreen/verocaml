[%%verocaml.symbolic val observed : int -> bool]

let missing_implementation_marker (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = 0];
  ()
[@@verocaml.axiom]
