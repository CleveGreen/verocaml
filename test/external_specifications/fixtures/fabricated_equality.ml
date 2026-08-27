let opaque (x : int) = x * x
let opaque_specification (x : int) = opaque x
[@@verocaml.external_specification]
let fabricated (x : int) =
  [%verocaml.ensures fun result -> result];
  opaque x = opaque x
