module Imported = Numeric_collision_dependency

module Numeric_collision_dependency : sig
  type carrier = int
  [@@verocaml.numeric_carrier
    { profile = "collision-local-profile";
      representation = "immediate";
      compatibility = [] }]

  val law : carrier -> int
  [@@verocaml.spec]
end

val local_operation : Numeric_collision_dependency.carrier -> int
[@@verocaml.numeric_role
  { carrier = Numeric_collision_dependency.carrier;
    role_schema = "numeric-role.v1";
    role = "local-collision-view";
    semantics = Numeric_collision_dependency.law;
    visibility = "opaque";
    reveal = false;
    inline = false }]

val imported_operation : Imported.carrier -> int
[@@verocaml.numeric_role
  { carrier = Imported.carrier;
    role_schema = "numeric-role.v1";
    role = "alias-import-view";
    semantics = Imported.law;
    visibility = "opaque";
    reveal = false;
    inline = false }]

open Numeric_source_provider.Nested

val opened_operation : carrier -> int
[@@verocaml.numeric_role
  { carrier = carrier;
    role_schema = "numeric-role.v1";
    role = "opened-import-view";
    semantics = law;
    visibility = "opaque";
    reveal = false;
    inline = false }]

module Enclosing : sig
  type carrier = int
  [@@verocaml.numeric_carrier
    { profile = "enclosing-profile";
      representation = "immediate";
      compatibility = [] }]

  val law : carrier -> int
  [@@verocaml.spec]

  module Child : sig
    val operation : carrier -> int
    [@@verocaml.numeric_role
      { carrier = carrier;
        role_schema = "numeric-role.v1";
        role = "enclosing-view";
        semantics = law;
        visibility = "opaque";
        reveal = false;
        inline = false }]
  end
end
