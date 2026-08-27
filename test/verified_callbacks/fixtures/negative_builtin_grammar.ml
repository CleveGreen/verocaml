let ordinary x =
  [%verocaml.requires true];
  [%verocaml.ensures fun result -> result = x];
  x

let rejected x =
  [%verocaml.requires call_requires (ordinary x)];
  [%verocaml.ensures fun result -> result = x];
  x
