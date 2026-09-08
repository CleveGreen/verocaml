let fail format =
  Printf.ksprintf (fun message -> prerr_endline message; exit 3) format

let span_to_string span =
  Printf.sprintf "%s:%d:%d-%d:%d" (Filename.basename span.Diagnostic.file)
    span.start_pos.line span.start_pos.column span.end_pos.line span.end_pos.column

let load filename =
  match Typedtree_lowering.lower_file filename with
  | Ok program -> program
  | Error diagnostic ->
      fail "%s @ %s" diagnostic.Diagnostic.code
        (span_to_string diagnostic.span)

let validate program =
  match Sst_validation.validate program with
  | Ok validated -> validated
  | Error error -> fail "%s" (Sst_validation.error_to_string error)

let lower_vir program =
  match Symbolic_executor.lower_program program with
  | Ok vir -> vir
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)

let dump_sst filename = print_string (Sst.to_string (load filename))
let dump_vir filename = print_string (Vir.to_string (lower_vir (load filename)))

let descriptors filename =
  let validated = validate (load filename) in
  List.iter
    (fun descriptor ->
      match Sst_validation.type_logical_type descriptor with
      | None -> ()
      | Some logical ->
          let type_id = Sst_validation.type_id descriptor in
          let nominal =
            match Sst_validation.logical_nominal_type logical with
            | Some nominal when nominal = type_id -> "nominal"
            | Some _ -> "wrong-nominal"
            | None -> "structural"
          in
          Printf.printf "logical-type %s#%d %s\n" type_id.type_name
            type_id.type_index nominal)
    (Sst_validation.type_descriptors validated);
  List.iter
    (fun model ->
      let callable =
        Sst_validation.model_callable model
        |> Sst_validation.callable_id
      in
      let domain =
        Sst_validation.model_domain model |> Sst_validation.type_id
      in
      let result =
        Sst_validation.model_result model
        |> Sst_validation.logical_source_type
        |> Sst.string_of_type
      in
      let visibility =
        match
          Sst_validation.model_visibility model
          |> Sst_validation.visibility_representation
        with
        | Sst.Abstract_with_evidence
            (Sst.Authenticated_same_cmt_abstraction _) ->
            "authenticated-abstract"
        | Sst.Revealed -> "revealed"
        | Sst.Abstract_with_evidence
            (Sst.Incomplete_abstraction_evidence _
            | Sst.Proposed_same_cmt_abstraction _) ->
            "unauthenticated"
      in
      Printf.printf "model %s#%d domain=%s#%d result=%s visibility=%s\n"
        callable.function_name callable.function_index domain.type_name
        domain.type_index result visibility)
    (Sst_validation.model_descriptors validated)

let solve filename =
  let vir = lower_vir (load filename) in
  let config =
    match Solver_backend.config ~timeout_ms:5000 with
    | Ok config -> config
    | Error error -> fail "%s" (Solver_backend.error_to_string error)
  in
  List.iter
    (fun execution ->
      match Solver_backend.solve_in_order config execution.Vir.obligations with
      | Error error -> fail "%s" (Solver_backend.error_to_string error)
      | Ok results ->
          if
            List.length results <> List.length execution.obligations
            || List.exists
                 (fun result ->
                   result.Solver_backend.outcome <> Solver_backend.Verified)
                 results
          then fail "%s did not verify" execution.function_ref.function_name
          else
            Printf.printf "%s: verified (%d obligations)\n"
              execution.function_ref.function_name
              (List.length execution.obligations))
    vir.functions

let reject filename =
  match Typedtree_lowering.lower_file filename with
  | Error diagnostic ->
      Printf.printf "adapter rejected: %s\n" diagnostic.Diagnostic.code
  | Ok program -> (
      match Symbolic_executor.lower_program program with
      | Error { Symbolic_executor.unsupported = Malformed_sst _; _ } ->
          print_endline "semantic validation rejected before VIR"
      | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
      | Ok _ -> fail "negative fixture was accepted")

let span line =
  let position column = Diagnostic.{ line; column } in
  Diagnostic.
    {
      file = "aggregate_specifications_raw.ml";
      start_pos = position 0;
      end_pos = position 1;
    }

let raw_type_id name index = Sst.{ type_index = index; type_name = name }

let raw_field owner index name line =
  Sst.
    {
      field_id =
        { field_owner = owner; field_index = index; field_name = name };
      field_type = Int;
      field_mutability = Immutable_field;
      field_modalities =
        {
          uniqueness_modality = Preserve_uniqueness;
          linearity_modality = Preserve_linearity;
        };
      span = span line;
    }

let raw_constructor constructor_id constructor_fields line =
  Sst.{ constructor_id; constructor_fields; span = span line }

let raw_type_definition type_id type_kind line =
  Sst.
    {
      type_id;
      type_kind;
      representation = Revealed;
      span = span line;
    }

let raw_type_program types =
  Sst.{ policy = Default_linear_z3; parametric_adts = []; types; logical_constants = []; functions = [] }

let expect_identity_rejection label line detail program =
  let expected_validation =
    Printf.sprintf
      "program: invalid semantic SST: %s at aggregate_specifications_raw.ml:%d:0-%d:1"
      detail line line
  in
  let validation =
    match Sst_validation.validate program with
    | Error error -> Sst_validation.error_to_string error
    | Ok _ -> fail "%s: validator accepted malformed nominal identity" label
  in
  if not (String.equal validation expected_validation) then
    fail "%s: unstable validator diagnostic: %s" label validation;
  Printf.printf "%s validator: %s\n" label validation;
  let expected_lowering =
    Printf.sprintf "program: malformed SST: %s at %s:%d:0-%d:1"
      expected_validation "aggregate_specifications_raw.ml" line line
  in
  let lowering =
    match Symbolic_executor.lower_program program with
    | Error error -> Symbolic_executor.error_to_string error
    | Ok _ -> fail "%s: malformed nominal identity reached VIR" label
  in
  if not (String.equal lowering expected_lowering) then
    fail "%s: unstable lowering diagnostic: %s" label lowering;
  Printf.printf "%s lowerer: %s\n" label lowering

let raw_identity_attacks () =
  let left_id = raw_type_id "left" 0 in
  let right_id = raw_type_id "right" 1 in
  let record owner index line =
    raw_type_definition left_id
      (Sst.Record_definition
         [ raw_field (Sst.Record_owner owner) index "value" line ])
      line
  in
  let empty_record type_id line =
    raw_type_definition type_id (Sst.Record_definition []) line
  in
  expect_identity_rejection "record-owner" 10
    "record field owner must match enclosing record type"
    (raw_type_program
       [ record right_id 0 10; empty_record right_id 11 ]);
  expect_identity_rejection "record-index" 12
    "record fields must have dense zero-based indices"
    (raw_type_program [ record left_id 1 12 ]);

  let constructor type_id index name fields line =
    let constructor_id =
      Sst.
        {
          constructor_type = type_id;
          constructor_index = index;
          constructor_name = name;
        }
    in
    raw_constructor constructor_id fields line
  in
  let variant constructor line =
    raw_type_definition left_id (Sst.Variant_definition [ constructor ]) line
  in
  expect_identity_rejection "constructor-owner" 20
    "variant constructor owner must match enclosing variant type"
    (raw_type_program
       [
         variant (constructor right_id 0 "Payload" [] 20) 20;
         raw_type_definition right_id (Sst.Variant_definition []) 21;
       ]);
  expect_identity_rejection "constructor-index" 22
    "variant constructors must have dense zero-based indices"
    (raw_type_program
       [ variant (constructor left_id 1 "Payload" [] 22) 22 ]);

  let payload_id =
    Sst.
      {
        constructor_type = left_id;
        constructor_index = 0;
        constructor_name = "Payload";
      }
  in
  let alternate_id =
    Sst.
      {
        constructor_type = left_id;
        constructor_index = 1;
        constructor_name = "Alternate";
      }
  in
  let payload field =
    raw_constructor payload_id [ field ] 30
  in
  expect_identity_rejection "constructor-field-owner" 30
    "constructor field owner must match enclosing variant constructor"
    (raw_type_program
       [
         raw_type_definition left_id
           (Sst.Variant_definition
              [
                payload
                  (raw_field (Sst.Constructor_owner alternate_id) 0 "$0" 30);
                raw_constructor alternate_id [] 31;
              ])
           30;
       ]);
  expect_identity_rejection "constructor-field-index" 32
    "constructor fields must have dense zero-based indices"
    (raw_type_program
       [
         raw_type_definition left_id
           (Sst.Variant_definition
              [
                payload
                  (raw_field (Sst.Constructor_owner payload_id) 1 "$0" 32);
              ])
           32;
       ])

let raw_attacks filename =
  let validated = load filename |> validate in
  let model =
    match Sst_validation.model_descriptors validated with
    | [ model ] -> model
    | models -> fail "expected one model descriptor, got %d" (List.length models)
  in
  let model_id =
    Sst_validation.model_callable model |> Sst_validation.callable_id
  in
  let wrong_index = { model_id with function_index = model_id.function_index + 1 } in
  let wrong_name = { model_id with function_name = model_id.function_name ^ ".forged" } in
  if
    Option.is_some (Sst_validation.find_model validated wrong_index)
    || Option.is_some (Sst_validation.find_model validated wrong_name)
  then fail "wrong model identity resolved";
  print_endline "rejected: wrong model index/name cannot resolve a descriptor";

  let type_id name index = Sst.{ type_index = index; type_name = name } in
  let field type_id =
    Sst.
      {
        field_id =
          {
            field_owner = Record_owner type_id;
            field_index = 0;
            field_name = "value";
          };
        field_type = Int;
        field_mutability = Immutable_field;
        field_modalities =
          {
            uniqueness_modality = Preserve_uniqueness;
            linearity_modality = Preserve_linearity;
          };
        span = span 1;
      }
  in
  let left_id = type_id "left" 0 and right_id = type_id "right" 1 in
  let left_field = field left_id and right_field = field right_id in
  let type_definition type_id field =
    Sst.
      {
        type_id;
        type_kind = Record_definition [ field ];
        representation = Revealed;
        span = span 1;
      }
  in
  let binding =
    Sst.
      {
        id = 0;
        name = "left";
        typ = Aggregate left_id;
        uniqueness = Definitely_aliased;
        span = span 2;
      }
  in
  let pattern =
    Sst.{ pattern_desc = Bind binding; typ = binding.typ; span = span 2 }
  in
  let variable =
    Sst.
      {
        expression_desc =
          Variable { binding; use_uniqueness = Definitely_aliased };
        typ = binding.typ;
        span = span 3;
      }
  in
  let body =
    Sst.
      {
        expression_desc = Field_read { record = variable; field = right_field.field_id };
        typ = Int;
        span = span 3;
      }
  in
  let definition =
    Sst.
      {
        function_id = { function_index = 0; function_name = "wrong_nominal" };
        type_binders = [];
        mode = Spec;
        recursive = false;
        parameters = [ Sst.Value_parameter
          { label = None; pattern; optional_default = None } ];
        contracts = empty_contracts;
        body = Spec_definition { stage = Logical; expression = body };
        policy = Default_linear_z3;
        result_type = Int;
        returns_unique_parameter = None;
        span = span 2;
      }
  in
  let wrong_nominal =
    Sst.
      {
        policy = Default_linear_z3;
        parametric_adts = [];
        types =
          [
            type_definition left_id left_field;
            type_definition right_id right_field;
          ];
        logical_constants = [];
        functions = [ definition ];
      }
  in
  (match Symbolic_executor.lower_program wrong_nominal with
  | Error { unsupported = Malformed_sst _; _ } ->
      print_endline "rejected: wrong nominal field domain before VIR"
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  | Ok _ -> fail "wrong nominal aggregate reached VIR");

  let owned_field =
    {
      left_field with
      field_modalities =
        {
          left_field.field_modalities with
          uniqueness_modality = Force_unique;
        };
    }
  in
  let owned_body = variable in
  let owned_definition =
    {
      definition with
      function_id = { function_index = 0; function_name = "owned_value" };
      body = Spec_definition { stage = Logical; expression = owned_body };
      result_type = Aggregate left_id;
    }
  in
  let owned_program =
    Sst.
      {
        policy = Default_linear_z3;
        parametric_adts = [];
        types = [ type_definition left_id owned_field ];
        logical_constants = [];
        functions = [ owned_definition ];
      }
  in
  (match Symbolic_executor.lower_program owned_program with
  | Error { unsupported = Malformed_sst _; _ } ->
      print_endline "rejected: ownership-bearing logical aggregate before VIR"
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  | Ok _ -> fail "ownership-bearing aggregate reached VIR");

  let program = Sst_validation.program validated in
  let abstract_domain = Sst_validation.model_domain model |> Sst_validation.type_id in
  let hidden_field =
    List.find_map
      (fun (definition : Sst.type_definition) ->
        if definition.type_id <> abstract_domain then None
        else
          match definition.type_kind with
          | Record_definition fields ->
              List.find_opt
                (fun (field : Sst.field_definition) -> field.field_type = Int)
                fields
          | Variant_definition _ -> None)
      program.types
  in
  let hidden_field =
    match hidden_field with
    | Some field -> field
    | None -> fail "abstract model domain has no scalar hidden field"
  in
  let observe =
    match List.rev program.functions with
    | observe :: _ -> observe
    | [] -> fail "model fixture has no observer"
  in
  let parameter =
    match observe.parameters with
    | [ Sst.Value_parameter
          { pattern = { pattern_desc = Bind binding; _ }; _ } ] -> binding
    | _ -> fail "observer does not have one bound parameter"
  in
  let receiver =
    Sst.
      {
        expression_desc =
          Variable { binding = parameter; use_uniqueness = Definitely_aliased };
        typ = parameter.typ;
        span = observe.span;
      }
  in
  let exposed =
    Sst.
      {
        expression_desc =
          Field_read { record = receiver; field = hidden_field.field_id };
        typ = hidden_field.field_type;
        span = observe.span;
      }
  in
  let forged_clause =
    Sst.
      {
        clause_index = 0;
        predicate =
          {
            stage = Logical;
            expression =
              {
                expression_desc =
                  Compare
                    ( Greater_or_equal,
                      exposed,
                      {
                        expression_desc = Int_constant Z.zero;
                        typ = Int;
                        span = observe.span;
                      } );
                typ = Bool;
                span = observe.span;
              };
          };
        span = observe.span;
      }
  in
  let forged_observe =
    {
      observe with
      contracts =
        { observe.contracts with requires = [ forged_clause ] };
    }
  in
  let forged =
    {
      program with
      functions =
        List.rev (forged_observe :: List.tl (List.rev program.functions));
    }
  in
  (match Symbolic_executor.lower_program forged with
  | Error { unsupported = Malformed_sst _; _ } ->
      print_endline
        "rejected: raw hidden-representation/model-opacity substitution before VIR"
  | Error error -> fail "%s" (Symbolic_executor.error_to_string error)
  | Ok _ -> fail "raw hidden representation substitution reached VIR")

let () =
  match Array.to_list Sys.argv with
  | [ _; "dump-sst"; filename ] -> dump_sst filename
  | [ _; "dump-vir"; filename ] -> dump_vir filename
  | [ _; "descriptors"; filename ] -> descriptors filename
  | [ _; "solve"; filename ] -> solve filename
  | [ _; "reject"; filename ] -> reject filename
  | [ _; "raw-identity-attacks" ] -> raw_identity_attacks ()
  | [ _; "raw-attacks"; filename ] -> raw_attacks filename
  | _ ->
      fail
        "usage: aggregate_specifications_tool \
         (dump-sst|dump-vir|descriptors|solve|reject|raw-attacks) FILE.cmt | \
         raw-identity-attacks"
