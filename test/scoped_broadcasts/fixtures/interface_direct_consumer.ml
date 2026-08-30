[@@@verocaml.activate
  [Interface_provider.zero_axiom; Interface_provider.nonnegative_axiom]]

let direct_integer_facts (value : int) : unit =
  [%verocaml.requires Interface_provider.is_zero value];
  [%verocaml.requires Interface_provider.is_nonnegative value];
  [%verocaml.assert value = 0];
  [%verocaml.assert value >= 0];
  ()
[@@verocaml.proof]
