type carrier = int
[@@verocaml.numeric_carrier
  { profile = "mismatch-profile";
    representation = "immediate";
    compatibility = [] }]

val law_a : carrier -> int
[@@verocaml.spec]

val law_b : carrier -> int
[@@verocaml.spec]

val operation : carrier -> int
[@@verocaml.numeric_role
  { carrier = carrier;
    role_schema = "numeric-role.v1";
    role = "mismatch-view";
    semantics = law_a;
    visibility = "opaque";
    reveal = false;
    inline = false }]
