type node = Empty | Node of int * node

let rec build choose count (xs : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then xs
  else
    let child = build choose (count - 1) xs in
    let selected = if choose then child else xs in
    Node (count, selected)

let consume (_node : node [@finite]) : unit = ()

let use choose count (xs : node [@finite]) =
  [%verocaml.requires count >= 0];
  consume (build choose count xs)
