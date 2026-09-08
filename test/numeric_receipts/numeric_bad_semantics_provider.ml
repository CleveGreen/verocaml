type carrier = int
[@@verocaml.numeric_carrier
  { profile = "portable-profile";
    representation = "immediate";
    compatibility = [] }]

let ordinary_relation value = value

let operation value = value
[@@verocaml.numeric_role
  { carrier = carrier;
    role_schema = "numeric-role.v1";
    role = "view";
    semantics = ordinary_relation;
    visibility = "opaque";
    reveal = false;
    inline = false }]
