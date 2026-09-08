type carrier = int
[@@verocaml.numeric_carrier
  { profile = "shadow-profile";
    representation = "immediate";
    compatibility = [] }]

module A : sig
  type marker = carrier
  val law : carrier -> int
  [@@verocaml.spec]
end

module B : sig
  type marker = carrier
  val law : carrier -> int
  [@@verocaml.spec]
end

open B

val operation : marker -> int
[@@verocaml.numeric_role
  { carrier = carrier;
    role_schema = "numeric-role.v1";
    role = "shadow-view";
    semantics = law;
    visibility = "opaque";
    reveal = false;
    inline = false }]
