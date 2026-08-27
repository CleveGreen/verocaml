let measure (value : int) : int =
  let nested = value in
  nested
[@@verocaml.spec]

let rec invalid_decrease (value : int) : int =
  [%verocaml.decreases (let nested = value in measure nested)];
  if value <= 0 then 0 else invalid_decrease (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
