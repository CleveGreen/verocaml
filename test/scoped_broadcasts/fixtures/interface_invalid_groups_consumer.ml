[@@@verocaml.activate [Interface_invalid_groups.invalid_external_target]]

let imports_invalid_group (value : int) : unit =
  [%verocaml.assert value = value];
  ()
[@@verocaml.proof]
