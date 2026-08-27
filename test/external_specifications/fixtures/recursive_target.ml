let rec target (x : int) = if x = 0 then 0 else target (x - 1)
let wrapper (x : int) = target x
[@@verocaml.external_specification]
