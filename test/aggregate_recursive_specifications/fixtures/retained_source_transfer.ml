type node = Empty | Node of int * node

let rec retained_source count (node : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then
    node
  else if count = 1 then
    Node (count, node)
  else
    retained_source (count - 1) node

let consume (_node : node [@finite]) : unit = ()

let use count (node : node [@finite]) : unit =
  [%verocaml.requires count >= 0];
  let result = retained_source count node in
  consume result
