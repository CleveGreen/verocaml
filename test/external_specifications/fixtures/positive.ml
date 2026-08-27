let promised (x : int) = x * x
let promised_specification (x : int) =
  [%verocaml.requires x < 100];
  [%verocaml.ensures fun result -> result = x + 1];
  promised x
[@@verocaml.external_specification]

let opaque (x : int) = x * x
let opaque_specification (x : int) = opaque x
[@@verocaml.external_specification]

let bool_source (x : bool) = not x
let bool_source_specification (x : bool) = bool_source x
[@@verocaml.external_specification]

let use_promised (x : int) =
  [%verocaml.requires x < 100];
  [%verocaml.ensures fun result -> result = x + 1];
  promised x

let use_havoc (x : int) =
  let first = opaque x in
  let second = opaque x in
  if first = second then first else second
