let span line =
  let position column = { Diagnostic.line; column } in
  {
    Diagnostic.file = "installed_forgery.ml";
    start_pos = position 0;
    end_pos = position 20;
  }

let expression line typ expression_desc =
  { Sst.expression_desc; typ; span = span line }

let () =
  let output = Sys.argv.(1) in
  let function_id = { Sst.function_index = 0; function_name = "forged" } in
  let x =
    {
      Sst.id = 0;
      name = "x";
      typ = Sst.Int;
      uniqueness = Sst.Definitely_aliased;
      span = span 1;
    }
  in
  let variable = expression 1 Sst.Int (Sst.Variable { binding = x; use_uniqueness = Sst.Definitely_aliased }) in
  let zero = expression 1 Sst.Int (Sst.Int_constant Z.zero) in
  let condition =
    expression 1 Sst.Bool (Sst.Compare (Sst.Less_or_equal, variable, zero))
  in
  let predecessor =
    expression 1 Sst.Int
      (Sst.Checked_arithmetic
         (Sst.Subtract, [ variable; expression 1 Sst.Int (Sst.Int_constant Z.one) ]))
  in
  let recursive_call =
    expression 1 Sst.Int
      (Sst.Direct_call
         {
           call_form = Sst.Specification_call;
           callee = function_id;
           type_arguments = [];
           arguments = [ Sst.Value_argument { label = None; value = predecessor } ];
           recursive = true;
         })
  in
  let body =
    expression 1 Sst.Int (Sst.If (condition, zero, Some recursive_call))
  in
  let declaration_span = span 1 in
  let definition =
    {
      Sst.function_id;
      mode = Sst.Spec;
      recursive = true;
      type_binders = [];
      parameters =
        [
          Sst.Value_parameter
            {
              Sst.label = None;
              pattern =
                { Sst.pattern_desc = Sst.Bind x; typ = Sst.Int; span = span 1 };
              optional_default = None;
            };
        ];
      contracts =
        {
          Sst.empty_contracts with
          decreases =
            [
              {
                Sst.clause_index = 0;
                predicate = { Sst.stage = Sst.Logical; expression = variable };
                span = span 1;
              };
            ];
        };
      body =
        Sst.Recursive_spec_definition
          {
            body = { Sst.stage = Sst.Logical; expression = body };
            visibility = `Opaque;
            provenance =
              Sst.Authenticated_typedtree
                {
                  source_file = "installed_forgery.ml";
                  declaration_span;
                };
          };
      policy = Sst.Default_linear_z3;
      result_type = Sst.Int;
      returns_unique_parameter = None;
      span = declaration_span;
    }
  in
  let program =
    {
      Sst.policy = Sst.Default_linear_z3;
      parametric_adts = [];
      types = [];
      logical_constants = [];
      functions = [ definition ];
    }
  in
  Solver_backend.For_testing.reset_solver_creation_count ();
  (match Sst_validation.validate program with
  | Error { Sst_validation.kind = Invalid_body _; _ } -> ()
  | Error error -> failwith (Sst_validation.error_to_string error)
  | Ok _ -> failwith "public recursive provenance tag forged admission");
  if Solver_backend.For_testing.solver_creation_count () <> 0 then
    failwith "installed recursive forgery created an smt.ml solver";
  let channel = open_out_bin output in
  Marshal.to_channel channel program [];
  close_out channel;
  print_endline "installed recursive SST tag rejected before solver"
