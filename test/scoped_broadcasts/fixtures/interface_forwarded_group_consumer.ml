[@@@verocaml.activate [Interface_groups.forwarded_integer_facts]]

let forwarded_group (value : int) : unit =
  [%verocaml.requires Interface_provider.is_zero value];
  [%verocaml.requires Interface_provider.is_nonnegative value];
  [%verocaml.assert value = 0];
  [%verocaml.assert value >= 0];
  ()
[@@verocaml.proof]
