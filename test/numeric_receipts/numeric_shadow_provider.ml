type carrier = int
[@@verocaml.numeric_carrier
  { profile = "shadow-profile";
    representation = "immediate";
    compatibility = [] }]

module A = struct
  type marker = carrier
  let law value = value
  [@@verocaml.spec]
end

module B = struct
  type marker = carrier
  let law value = value + 1
  [@@verocaml.spec]
end

open A

let operation (value : marker) = law value
[@@verocaml.numeric_role
  { carrier = carrier;
    role_schema = "numeric-role.v1";
    role = "shadow-view";
    semantics = law;
    visibility = "opaque";
    reveal = false;
    inline = false }]
