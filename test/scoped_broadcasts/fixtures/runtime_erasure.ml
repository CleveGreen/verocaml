let observed (_value : int) : bool = true [@@verocaml.spec]

let lemma_runtime (value : int) : unit =
  [%verocaml.ensures fun _ -> ((observed value) [@trigger]) || value = value];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]

[@@@verocaml.broadcast_group (runtime_group, [lemma_runtime])]
[@@@verocaml.activate [runtime_group]]

let compute value =
  [%verocaml.activate [lemma_runtime] (value + 1)]

let () = Printf.printf "runtime=%d\n" (compute 41)
