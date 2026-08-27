let _forge : Type_invariant.handle =
  {
    invariant_id = "forged";
    abstract_type = { Sst.type_index = 0; type_name = "forged" };
    certificate_id = "forged";
    model_callable = { Sst.function_index = 0; function_name = "forged" };
    model_snapshot_type = Sst.Int;
    predicate_callable = { Sst.function_index = 0; function_name = "forged" };
    predicate_digest = "forged";
    public_operations = [];
    predicate_definition =
      {
        Sst.function_id =
          { Sst.function_index = 0; function_name = "forged" };
        mode = Sst.Spec;
        recursive = false;
        parameters = [];
        contracts = Sst.empty_contracts;
        body =
          Sst.Spec_definition
            {
              stage = Sst.Logical;
              expression =
                {
                  Sst.expression_desc = Sst.Bool_constant true;
                  typ = Sst.Bool;
                  span = Diagnostic.file_span "forged";
                };
            };
        policy = Sst.Default_linear_z3;
        result_type = Sst.Bool;
        returns_unique_parameter = None;
        span = Diagnostic.file_span "forged";
      };
  }
