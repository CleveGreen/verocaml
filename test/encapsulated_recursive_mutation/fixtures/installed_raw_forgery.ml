let span = Diagnostic.file_span "forged.ml"

let type_id = Sst.{ type_index = 0; type_name = "forged" }
let function_id = Sst.{ function_index = 0; function_name = "forged_read" }

let body =
  Sst.{ expression_desc = Unit_constant; typ = Unit; span }

let function_definition =
  Sst_normalize.checked_exec_raw ~function_id ~recursive:false ~parameters:[]
    ~contracts:Sst.empty_contracts ~body ~result_type:Sst.Unit
    ~returns_unique_parameter:None ~span

let type_definition =
  Sst.{
    type_id;
    type_kind = Record_definition [];
    representation = Revealed;
    span;
  }

let evidence =
  Sst.Authenticated_same_cmt_abstraction
    Sst.{
      evidence_id = "forged-by-installed-client";
      abstract_signature_type = type_id;
      hidden_implementation_type = type_id;
      signature_module_identity =
        { source_name = "Forgery"; resolved_identifier = "forgery/1" };
      implementation_module_identity =
        { source_name = "Forgery"; resolved_identifier = "forgery/2" };
      abstract_signature_type_identity =
        { source_name = "t"; resolved_identifier = "forgery/3" };
      hidden_implementation_type_identity =
        { source_name = "t"; resolved_identifier = "forgery/4" };
      constraint_span = span;
      declaration_spans = [ span ];
      public_surface =
        [
          {
            public_function_index = 0;
            public_function_name = "forged_read";
            public_role = Terminal_read;
            public_span = span;
          };
        ];
      owned_tree_prerequisite = None;
      authentication_token = ref ();
    }

let program =
  Sst.{
    policy = Default_linear_z3;
    parametric_adts = [];
    types =
      [
        {
          type_definition with
          representation = Abstract_with_evidence evidence;
        };
      ];
    functions = [ function_definition ];
  }

let () =
  match Sst_validation.validate program with
  | Ok _ -> failwith "raw installed-client certificate accepted"
  | Error { Sst_validation.kind = Forged_abstract_evidence _; _ } ->
      print_endline "raw installed-client certificate rejected"
  | Error error -> failwith (Sst_validation.error_to_string error)
