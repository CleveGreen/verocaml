type 'a measure = int
[@@verocaml.numeric_carrier
  { profile = "portable-profile";
    representation = "boxed";
    compatibility = [ "word-layout" ] }]

val relation : 'a measure -> int
[@@verocaml.spec]

val operation : 'a measure -> int
[@@verocaml.numeric_role
  { carrier = measure;
    role_schema = "numeric-role.v1";
    role = "view";
    semantics = relation;
    visibility = "opaque";
    reveal = false;
    inline = false }]

module Nested : sig
  type carrier = int
  [@@verocaml.numeric_carrier
    { profile = "nested-profile";
      representation = "immediate";
      compatibility = [] }]

  val law : carrier -> int
  [@@verocaml.spec]

  val operation : carrier -> int
  [@@verocaml.numeric_role
    { carrier = carrier;
      role_schema = "numeric-role.v1";
      role = "view";
      semantics = law;
      visibility = "opaque";
      reveal = false;
      inline = false }]
end
