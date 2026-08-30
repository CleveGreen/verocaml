[@@@verocaml.activate [Interface_groups.forwarded_option_facts]]

let unwrap_int (value : int option) : unit =
  [%verocaml.requires Interface_provider.option_present value];
  [%verocaml.assert Interface_provider.option_unwraps value];
  ()
[@@verocaml.proof]

let unwrap_bool (value : bool option) : unit =
  [%verocaml.requires Interface_provider.option_present value];
  [%verocaml.assert Interface_provider.option_unwraps value];
  ()
[@@verocaml.proof]
