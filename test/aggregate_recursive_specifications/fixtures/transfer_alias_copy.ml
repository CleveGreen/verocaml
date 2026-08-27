type node = Empty | Node of int * node

let rec copied count (node : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then node
  else
    let result = copied (count - 1) node in
    match result with
    | Empty -> Empty
    | Node (value, next) -> Node (value, next)

let consume (_node : node [@finite]) = ()

let use count (node : node [@finite]) =
  [%verocaml.requires count >= 0];
  consume (copied count node)
