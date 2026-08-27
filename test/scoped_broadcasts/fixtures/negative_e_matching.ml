let observed_pair (_left : int) (_right : int) : bool =
  true
[@@verocaml.spec]

let false_pair (left : int) (right : int) : unit =
  [%verocaml.ensures fun _ ->
    ((observed_pair left right) [@trigger]) && left = right];
  ()
[@@verocaml.proof]
[@@verocaml.broadcast]
