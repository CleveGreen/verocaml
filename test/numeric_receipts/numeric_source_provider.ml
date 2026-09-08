type 'a measure = int
[@@verocaml.numeric_carrier
  { profile = "portable-profile";
    representation = "boxed";
    compatibility = [ "word-layout" ] }]

let relation value = value
[@@verocaml.spec]

let operation value = value
[@@verocaml.numeric_role
  { carrier = measure;
    role_schema = "numeric-role.v1";
    role = "view";
    semantics = relation;
    visibility = "opaque";
    reveal = false;
    inline = false }]

module Nested = struct
  type carrier = int
  [@@verocaml.numeric_carrier
    { profile = "nested-profile";
      representation = "immediate";
      compatibility = [] }]

  let law value = value
  [@@verocaml.spec]

  let operation value = value
  [@@verocaml.numeric_role
    { carrier = carrier;
      role_schema = "numeric-role.v1";
      role = "view";
      semantics = law;
      visibility = "opaque";
      reveal = false;
      inline = false }]
end
