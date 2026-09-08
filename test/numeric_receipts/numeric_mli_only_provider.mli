type carrier = int
[@@verocaml.numeric_carrier
  { profile = "mli-only-profile";
    representation = "immediate";
    compatibility = [] }]

val law : carrier -> int
[@@verocaml.spec]

val operation : carrier -> int
[@@verocaml.numeric_role
  { carrier = carrier;
    role_schema = "numeric-role.v1";
    role = "mli-only-view";
    semantics = law;
    visibility = "opaque";
    reveal = false;
    inline = false }]
