let run () =
  [%verocaml.ensures fun result -> result = 0];
  let left = Retained_simple_bindings_provider.Node (1, Retained_simple_bindings_provider.Empty) in
  let right = Retained_simple_bindings_provider.Node (2, Retained_simple_bindings_provider.Empty) in
  Retained_simple_bindings_provider.checked left right
