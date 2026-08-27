type node = Empty | Node of int * node

let rec build count (xs : node) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then xs
  else Node (count, build (count - 1) xs)

let consume (_node : node [@finite]) : unit = ()

let use count (xs : node) =
  [%verocaml.requires count >= 0];
  consume (build count xs)
