type node = Empty | Node of int * node

let rec projected count (node : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then node
  else
    match projected (count - 1) node with
    | Empty -> Empty
    | Node (_, next) -> next

let consume (_node : node [@finite]) = ()

let use count (node : node [@finite]) =
  [%verocaml.requires count >= 0];
  consume (projected count node)
