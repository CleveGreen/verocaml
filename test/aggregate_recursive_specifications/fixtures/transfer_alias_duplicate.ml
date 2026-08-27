type node = Empty | Node of int * node

let rec duplicated count (node : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then node
  else
    let result = duplicated (count - 1) node in
    if result = result then result else result

let consume (_node : node [@finite]) = ()

let use count (node : node [@finite]) =
  [%verocaml.requires count >= 0];
  consume (duplicated count node)
