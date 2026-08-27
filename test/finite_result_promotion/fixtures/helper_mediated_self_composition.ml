type node = Empty | Node of int * node

let wrap value child = Node (value, child)

let rec build count (xs : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then xs else wrap count (build (count - 1) xs)

let consume (_node : node [@finite]) : unit = ()

let use count (xs : node [@finite]) =
  [%verocaml.requires count >= 0];
  consume (build count xs)
