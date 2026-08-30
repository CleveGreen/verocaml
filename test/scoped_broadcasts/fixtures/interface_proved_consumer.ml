[@@@verocaml.activate
  [Interface_groups.selected_proved_facts;
   Interface_groups.forwarded_proved_facts]]

let proved_int (value : int) : unit =
  [%verocaml.requires Interface_provider.reflexive_probe value];
  [%verocaml.assert value = value];
  ()
[@@verocaml.proof]

let proved_bool (value : bool) : unit =
  [%verocaml.requires Interface_provider.reflexive_probe value];
  [%verocaml.assert value = value];
  ()
[@@verocaml.proof]
