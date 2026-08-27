[%%verocaml.symbolic val observed : int -> bool]

let zero_axiom (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = 0];
  ()
[@@verocaml.axiom] [@@verocaml.broadcast]

let active (value : int) : unit =
  [%verocaml.requires observed value];
  [%verocaml.activate [zero_axiom] ([%verocaml.assert value = 0]; ())]
[@@verocaml.proof]
