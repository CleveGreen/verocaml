type 'a pair = { left : 'a; right : 'a }
let swapped (left : int) (right : int) =
  [%verocaml.requires left <> right];
  [%verocaml.ensures fun result -> result];
  let record = { left; right } in
  record.left = right && record.right = left
