let promised (x : int) = x
let promised_specification (x : int) =
  [%verocaml.requires x > 0];
  promised x
[@@verocaml.external_specification]
let caller (x : int) = promised x
