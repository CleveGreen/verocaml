type node = Empty | Node of int * node

let rec rebound count (node : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then node
  else
    let result = rebound (count - 1) node in
    let rebound_result = result in
    rebound_result

let consume (_node : node [@finite]) = ()

let use count (node : node [@finite]) =
  [%verocaml.requires count >= 0];
  consume (rebound count node)
