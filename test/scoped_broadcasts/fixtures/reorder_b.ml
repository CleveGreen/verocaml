let observed_a (_value : int) : bool = true [@@verocaml.spec]
let observed_b (_value : bool) : bool = true [@@verocaml.spec]

let lemma_second (renamed : bool) : unit =
  [%verocaml.ensures fun _ ->
    ((observed_b renamed) [@trigger]) || renamed = renamed];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

let lemma_first (renamed : int) : unit =
  [%verocaml.ensures fun _ ->
    ((observed_a renamed) [@trigger]) || renamed = renamed];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

[@@@verocaml.activate [lemma_first; lemma_second]]

let use (number : int) (flag : bool) : unit =
  [%verocaml.requires observed_a number];
  [%verocaml.requires observed_b flag];
  [%verocaml.requires
    forall (fun candidate ->
      ((observed_a candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.requires
    forall (fun candidate ->
      ((observed_b candidate) [@trigger]) || candidate = candidate)];
  [%verocaml.assert number = number && flag = flag];
  ()
[@@verocaml.proof]
