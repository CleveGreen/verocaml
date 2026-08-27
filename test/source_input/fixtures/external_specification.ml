let opaque (x : int) : int = x + 1

let opaque_specification (x : int) : int =
  [%verocaml.requires x >= 0];
  [%verocaml.ensures fun result -> result >= 0];
  opaque x
[@@verocaml.external_specification]

let caller (x : int) : int =
  [%verocaml.requires x >= 0];
  opaque x
