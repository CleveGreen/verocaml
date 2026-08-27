let unit_helper (_value : int) : unit = () [@@verocaml.spec]

let rec invalid_unit (value : int) : int =
  [%verocaml.decreases value];
  let _ = unit_helper value in
  if value <= 0 then 0 else invalid_unit (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
