let forge () =
  {
    Interface_specification.issuer = ref ();
    unit_name = "Forged";
    interface_digest = "not-a-digest";
    types = [];
    callables = [];
    models = [];
    direct_dependencies = [];
    transitive_dependencies = [];
  }
