type node = Empty | Node of holder
and holder = { value : int; next : node }

let rec left count (xs : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then Node { value = 0; next = xs }
  else Node { value = count; next = right (count - 1) xs }

and right count (xs : node [@finite]) : node =
  [%verocaml.requires count >= 0];
  [%verocaml.decreases count];
  if count = 0 then Node { value = 0; next = xs }
  else Node { value = count; next = left (count - 1) xs }

let consume (_xs : node [@finite]) : unit = ()

let use count (xs : node [@finite]) : unit =
  [%verocaml.requires count >= 0];
  consume (left count xs)
