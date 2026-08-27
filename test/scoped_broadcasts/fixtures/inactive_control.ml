let observed (_value : int) : bool = true [@@verocaml.spec]

let axiom_zero (value : int) : unit =
  [%verocaml.ensures fun _ ->
    (not ((observed value) [@trigger])) || value = 0];
  ()
[@@verocaml.proof]
[@@verocaml.external_body]
[@@verocaml.broadcast]

let inactive (value : int) : unit =
  [%verocaml.requires observed value];
  [%verocaml.requires
    forall (fun candidate ->
      ((observed candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.assert value = 0];
  ()
[@@verocaml.proof]
