[@@@verocaml.broadcast_group
  (selected_integer_facts,
   [Interface_provider.zero_axiom; Interface_provider.nonnegative_axiom])]

[@@@verocaml.broadcast_group
  (forwarded_integer_facts, [Interface_provider.integer_facts])]

[@@@verocaml.broadcast_group
  (forwarded_option_facts, [Interface_provider.option_facts])]

[@@@verocaml.broadcast_group
  (forwarded_nested_facts, [Interface_provider.nested_facts])]

[@@@verocaml.broadcast_group
  (selected_proved_facts, [Interface_provider.reflexive_lemma])]

[@@@verocaml.broadcast_group
  (forwarded_proved_facts, [Interface_provider.proved_facts])]
