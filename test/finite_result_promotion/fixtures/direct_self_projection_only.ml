type node = Empty | Node of int * node

let rec build count (xs : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then xs
  else
    let child = build (count - 1) xs in
    match child with Empty -> Empty | Node (_, next) -> next

let consume (_node : node [@finite]) : unit = ()

let use count (xs : node [@finite]) =
  [%verocaml.requires count >= 0];
  consume (build count xs)
