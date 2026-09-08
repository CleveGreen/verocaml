type carrier = int

let law_a value = value
[@@verocaml.spec]

let law_b value = value
[@@verocaml.spec]

let operation value = value
[@@verocaml.numeric_role
  { carrier = carrier;
    role_schema = "numeric-role.v1";
    role = "mismatch-view";
    semantics = law_b;
    visibility = "opaque";
    reveal = false;
    inline = false }]
