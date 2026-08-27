type node = Empty | Node of int * node
let checked (value : node [@finite]) =
  [%verocaml.requires false];
  ()
let caller () = checked (Node (1, Empty))
