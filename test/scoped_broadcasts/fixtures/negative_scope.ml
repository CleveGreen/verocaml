let observed (_value : int) : bool = true [@@verocaml.spec]

let lemma_scope (value : int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger])];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

[@@@verocaml.activate [if true then lemma_scope else lemma_scope]]

let untouched value = value
