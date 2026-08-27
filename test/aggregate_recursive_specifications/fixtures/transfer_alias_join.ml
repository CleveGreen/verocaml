type node = Empty | Node of int * node

let rec joined count (node : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then node
  else
    let result = joined (count - 1) node in
    if count = 1 then result else result

let consume (_node : node [@finite]) = ()

let use count (node : node [@finite]) =
  [%verocaml.requires count >= 0];
  consume (joined count node)
