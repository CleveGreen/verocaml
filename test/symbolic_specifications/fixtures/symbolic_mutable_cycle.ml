type cycle = { mutable next : cycle option }

[%%verocaml.symbolic val bad : cycle -> cycle]
