val operation : int Numeric_source_provider.measure -> int
[@@verocaml.numeric_role
  { carrier = Numeric_source_provider.measure;
    role_schema = "numeric-role.v1";
    role = "extended-view";
    semantics = Numeric_source_provider.relation;
    visibility = "opaque";
    reveal = false;
    inline = false }]
