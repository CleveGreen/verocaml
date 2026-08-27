let observed_a (_value : int) : bool = true [@@verocaml.spec]
let observed_b (_value : int) : bool = true [@@verocaml.spec]

let lemma_a (value : int) : unit =
  [%verocaml.ensures fun _ -> ((observed_a value) [@trigger]) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

let lemma_b (value : int) : unit =
  [%verocaml.ensures fun _ -> ((observed_b value) [@trigger]) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

[@@@verocaml.broadcast_group (inner, [lemma_b; lemma_a])]
[@@@verocaml.broadcast_group (outer, [inner; lemma_a])]
[@@@verocaml.activate [outer; lemma_b]]

let grouped (value : int) : unit =
  [%verocaml.requires observed_a value];
  [%verocaml.requires observed_b value];
  [%verocaml.requires
    forall (fun candidate ->
      ((observed_a candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.requires
    forall (fun candidate ->
      ((observed_b candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.assert value = value];
  ()
[@@verocaml.proof]
