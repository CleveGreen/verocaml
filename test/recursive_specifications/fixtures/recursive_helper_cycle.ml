let rec cycle_left (value : int) : bool =
  [%verocaml.decreases value];
  if value <= 0 then true else cycle_right (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]

and cycle_right (value : int) : bool =
  [%verocaml.decreases value];
  if value <= 0 then true else cycle_left (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]

let rec invalid_cycle (value : int) : int =
  [%verocaml.decreases value];
  if cycle_left value then 0 else invalid_cycle (value - 1)
[@@verocaml.spec] [@@verocaml.opaque]
