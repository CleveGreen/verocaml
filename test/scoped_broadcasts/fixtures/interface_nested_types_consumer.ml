[@@@verocaml.activate [Interface_groups.forwarded_nested_facts]]

let nested_int_bool
    (value : ((int Interface_provider.box option) option, bool) result) : unit =
  [%verocaml.requires Interface_provider.nested_present value];
  [%verocaml.assert Interface_provider.nested_fact value];
  ()
[@@verocaml.proof]

let nested_bool_box_error
    (value :
      ((bool Interface_provider.box option) option,
       int Interface_provider.box option)
      result) : unit =
  [%verocaml.requires Interface_provider.nested_present value];
  [%verocaml.assert Interface_provider.nested_fact value];
  ()
[@@verocaml.proof]
