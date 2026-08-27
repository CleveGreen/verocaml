let trusted (x : int) =
  [%verocaml.requires x > 0];
  [%verocaml.ensures fun result -> result = x];
  x
[@@verocaml.external_body]
let caller () =
  [%verocaml.ensures fun result -> result = 0];
  trusted 0
