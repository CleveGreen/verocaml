type provider_identity = {
  unit_name : string;
  interface_digest : string;
  source_digest : string;
  family_digest : string;
  import_digest : string;
}

type provider_type = {
  resolved_path : string;
  source_path : string;
  source_name : string;
  binding_uid : string;
  definition : Sst.type_definition;
  parametric_descriptor : Parametric_adt.t option;
  revealed : bool;
  logical : bool;
  field_modes : (Sst.field_id * Sst.instance_mode) list;
  rank_profile_digest : string option;
}

type model = { domain : Sst.type_id; result_type : Sst.typ }
type model_closure = {
  closure_type_ids : Sst.type_id list;
  closure_digest : string;
}

type classification =
  | Retain_existing
  | Contract_only_exec
  | Opaque_model_spec of model_closure
  | Ineligible

type result_boundary =
  | Ghost_summary
  | Executable_boundary
  | Tracked_boundary
  | Unsupported_boundary

type schema_support = {
  supported : bool;
  contains_mathematical_int : bool;
}

let digest value = Digest.string value |> Digest.to_hex

let same_type_id left right =
  left.Sst.type_index = right.Sst.type_index
  && String.equal left.type_name right.type_name

let fields_of_definition definition =
  match definition.Sst.type_kind with
  | Sst.Record_definition fields -> fields
  | Variant_definition constructors ->
      List.concat_map
        (fun (constructor : Sst.constructor_definition) ->
          constructor.constructor_fields)
        constructors

let first_tranche_field_mode typ field =
  if
    field.Sst.field_mutability = Sst.Immutable_field
    && field.field_modalities.uniqueness_modality = Sst.Preserve_uniqueness
    && field.field_modalities.linearity_modality = Sst.Preserve_linearity
  then
    match List.assoc_opt field.field_id typ.field_modes with
    | Some (Sst.Exec_instance | Sst.Ghost_instance) as mode -> mode
    | Some Sst.Tracked_instance | None -> None
  else None

let provider_type types type_id =
  List.find_opt
    (fun typ -> same_type_id typ.definition.Sst.type_id type_id)
    types

let parametric_fields descriptor =
  match Parametric_adt.kind descriptor with
  | Parametric_adt.Record fields -> fields
  | Variant constructors ->
      List.concat_map
        (fun (constructor : Parametric_adt.constructor) ->
          constructor.constructor_fields)
        constructors

let schema_for_application parametric_adts = function
  | Sst.Application (constructor, arguments) as application -> (
      match Parametric_adt.find parametric_adts constructor with
      | Some descriptor when Parametric_adt.same_application descriptor application
        -> (
          match
            Logical_adt_schema_private.instantiate
              ~descriptors:parametric_adts
              ~applications:
                [ ((Parametric_adt.type_id descriptor).type_index, arguments) ]
          with
          | Ok schemas ->
              List.find_opt
                (fun schema ->
                  Parametric_type.compare_constructor
                    (Parametric_adt.type_constructor
                       (Logical_adt_schema_private.descriptor schema))
                    constructor
                  = 0
                  && List.compare Parametric_type.compare
                       (Logical_adt_schema_private.arguments schema)
                       arguments
                     = 0)
                schemas
          | Error _ -> None)
      | Some _ | None -> None)
  | Sst.Unit | Bool | Int | Mathematical_int | Bit_vector _ | Tuple _ | Aggregate _
  | Parameter _ ->
      None

let result_boundary definition result_mode =
  match (definition.Sst.mode, result_mode) with
  | Sst.Exec, (Sst.Exec_instance | Sst.Tracked_instance | Sst.Ghost_instance) ->
      Executable_boundary
  | (Sst.Spec | Sst.Proof), Sst.Exec_instance -> Executable_boundary
  | (Sst.Spec | Sst.Proof), Sst.Tracked_instance -> Tracked_boundary
  | (Sst.Spec | Sst.Proof), Sst.Ghost_instance -> (
      match (definition.mode, definition.recursive, definition.body) with
      | Sst.Spec, false, Sst.Spec_definition body
        when body.stage = Sst.Logical ->
          Ghost_summary
      | Sst.Spec, true, Sst.Recursive_spec_definition { body; _ }
        when body.stage = Sst.Logical ->
          Ghost_summary
      | Sst.Proof, _, Sst.Proof_body { body; _ }
        when body.stage = Sst.Proof_stage ->
          Ghost_summary
      | Sst.Proof, false, Sst.Trusted_external_body _ -> Ghost_summary
      | ( (Sst.Spec | Sst.Proof | Sst.Exec),
          _,
          ( Sst.Checked_exec _ | Sst.Spec_definition _
          | Sst.Recursive_spec_definition _ | Sst.Proof_body _
          | Sst.External_specification _ | Sst.Trusted_external_spec_target _
          | Sst.Trusted_external_body _ | Sst.Symbolic_declaration _ ) ) ->
          Unsupported_boundary)

let nested_boundary parent mode =
  match (parent, mode) with
  | Ghost_summary, Sst.Ghost_instance -> Ghost_summary
  | Ghost_summary, Sst.Exec_instance -> Executable_boundary
  | Executable_boundary, (Sst.Exec_instance | Sst.Ghost_instance) ->
      Executable_boundary
  | Tracked_boundary, (Sst.Exec_instance | Sst.Ghost_instance) ->
      Tracked_boundary
  | Unsupported_boundary, (Sst.Exec_instance | Sst.Ghost_instance) ->
      Unsupported_boundary
  | ( Ghost_summary | Executable_boundary | Tracked_boundary
    | Unsupported_boundary ),
    Sst.Tracked_instance ->
      Tracked_boundary

let same_visit_state (left_id, left_boundary) (right_id, right_boundary) =
  same_type_id left_id right_id && left_boundary = right_boundary

let schema_supported ~types ~parametric_adts ~logical_required ~boundary typ =
  let[@log_value.debug] result_boundary_name = function
    | Ghost_summary -> "ghost-summary"
    | Executable_boundary -> "executable-boundary"
    | Tracked_boundary -> "tracked-boundary"
    | Unsupported_boundary -> "unsupported-boundary"
  in
  let contains_mathematical_int = ref false in
  let rec visit aggregate_visiting application_visiting boundary = function
    | Sst.Unit | Bool | Int | Bit_vector _ | Parameter _ -> true
    | Mathematical_int ->
        contains_mathematical_int := true;
        let permitted = boundary = Ghost_summary in
        [%log.trace "classified retained mathematical result occurrence"
          ~stage:(Delator.Field.string "retained-result-schema")
          ~boundary:
            (Delator.Field.string
               ((result_boundary_name [@log_value.debug]) boundary))
          ~decision:
            (Delator.Field.string (if permitted then "accepted" else "rejected"))
          ~reason_class:
            (Delator.Field.string
               (if permitted then "ghost-summary"
                else "non-ghost-result-boundary"))];
        permitted
    | Tuple components ->
        List.for_all
          (fun (_, typ) ->
            visit aggregate_visiting application_visiting boundary typ)
          components
    | Aggregate type_id ->
        visit_aggregate aggregate_visiting application_visiting boundary type_id
    | Application (_, arguments) as application ->
        visit_application aggregate_visiting application_visiting boundary
          application arguments
  and visit_aggregate aggregate_visiting application_visiting boundary type_id =
    let state = (type_id, boundary) in
    if List.exists (same_visit_state state) aggregate_visiting then true
    else
      match provider_type types type_id with
      | None ->
          [%log.trace "rejected retained result with unavailable aggregate schema"
            ~stage:(Delator.Field.string "retained-result-schema")
            ~boundary:
              (Delator.Field.string
                 ((result_boundary_name [@log_value.debug]) boundary))
            ~type_name:(Delator.Field.string type_id.type_name)
            ~decision:(Delator.Field.string "rejected")
            ~reason_class:(Delator.Field.string "missing-provider-type")];
          false
      | Some typ ->
          typ.revealed
          && typ.definition.representation = Sst.Revealed
          && ((not logical_required) || typ.logical)
          && List.for_all
               (fun field ->
                 match first_tranche_field_mode typ field with
                 | None ->
                     [%log.trace "rejected retained result aggregate field"
                       ~stage:(Delator.Field.string "retained-result-schema")
                       ~boundary:
                         (Delator.Field.string
                            ((result_boundary_name [@log_value.debug]) boundary))
                       ~type_name:(Delator.Field.string type_id.type_name)
                       ~field_name:
                         (Delator.Field.string field.Sst.field_id.field_name)
                       ~decision:(Delator.Field.string "rejected")
                       ~reason_class:
                         (Delator.Field.string "unsupported-field-mode-or-layout")];
                     false
                 | Some mode ->
                     let nested = nested_boundary boundary mode in
                     [%log.trace "traversing retained result aggregate field"
                       ~stage:(Delator.Field.string "retained-result-schema")
                       ~boundary:
                         (Delator.Field.string
                            ((result_boundary_name [@log_value.debug]) boundary))
                       ~nested_boundary:
                         (Delator.Field.string
                            ((result_boundary_name [@log_value.debug]) nested))
                       ~type_name:(Delator.Field.string type_id.type_name)
                       ~field_name:
                         (Delator.Field.string field.Sst.field_id.field_name)
                       ~decision:(Delator.Field.string "traverse")];
                     visit (state :: aggregate_visiting) application_visiting
                       nested field.field_type)
               (fields_of_definition typ.definition)
  and visit_application aggregate_visiting application_visiting boundary
      application arguments =
    match schema_for_application parametric_adts application with
    | None ->
        [%log.trace "rejected retained result application schema"
          ~stage:(Delator.Field.string "retained-result-schema")
          ~boundary:
            (Delator.Field.string
               ((result_boundary_name [@log_value.debug]) boundary))
          ~decision:(Delator.Field.string "rejected")
          ~reason_class:
            (Delator.Field.string "missing-parametric-application-schema")];
        false
    | Some schema ->
        let application_id = Logical_adt_schema_private.application_id schema in
        let state = (application_id, boundary) in
        if List.mem state application_visiting then true
        else
          let descriptor = Logical_adt_schema_private.descriptor schema in
          let visit_field field =
            match Parametric_adt.instantiate_field descriptor arguments field with
            | Error _ ->
                [%log.trace "rejected retained result application field"
                  ~stage:(Delator.Field.string "retained-result-schema")
                  ~boundary:
                    (Delator.Field.string
                       ((result_boundary_name [@log_value.debug]) boundary))
                  ~application:(Delator.Field.string application_id)
                  ~field_name:
                    (Delator.Field.string field.Parametric_adt.field_name)
                  ~decision:(Delator.Field.string "rejected")
                  ~reason_class:
                    (Delator.Field.string "field-instantiation-failed")];
                false
            | Ok field_type ->
                [%log.trace "traversing retained result application field"
                  ~stage:(Delator.Field.string "retained-result-schema")
                  ~boundary:
                    (Delator.Field.string
                       ((result_boundary_name [@log_value.debug]) boundary))
                  ~application:(Delator.Field.string application_id)
                  ~field_name:
                    (Delator.Field.string field.Parametric_adt.field_name)
                  ~decision:(Delator.Field.string "traverse")];
                visit aggregate_visiting (state :: application_visiting)
                  boundary field_type
          in
          List.for_all
            (visit aggregate_visiting application_visiting boundary)
            arguments
          && List.for_all visit_field (parametric_fields descriptor)
  in
  let supported = visit [] [] boundary typ in
  { supported; contains_mathematical_int = !contains_mathematical_int }

let aggregate_bearing typ =
  let rec contains = function
    | Sst.Aggregate _ | Application _ -> true
    | Tuple components -> List.exists (fun (_, typ) -> contains typ) components
    | Unit | Bool | Int | Mathematical_int | Bit_vector _ | Parameter _ -> false
  in
  contains typ

let model_result_identity parametric_adts = function
  | Sst.Aggregate result ->
      Some [ result.type_name; string_of_int result.type_index ]
  | Sst.Application _ as application ->
      Option.map
        (fun schema ->
          [ Parametric_adt.compiler_uid
              (Logical_adt_schema_private.descriptor schema);
            Logical_adt_schema_private.application_id schema ])
        (schema_for_application parametric_adts application)
  | Sst.Unit | Bool | Int | Mathematical_int | Bit_vector _ | Tuple _ | Parameter _ -> None

let classify ~provider:(provider [@delator.skip])
    ~types:(types [@delator.skip])
    ~parametric_adts:(parametric_adts [@delator.skip])
    ~resolved_path:(resolved_path [@delator.skip])
    ~binding_uid:(binding_uid [@delator.skip])
    ~definition:(definition [@delator.skip])
    ~result_mode:(result_mode [@delator.skip])
    ~model:(model [@delator.skip]) =
  let[@log_value.debug] result_boundary_name = function
    | Ghost_summary -> "ghost-summary"
    | Executable_boundary -> "executable-boundary"
    | Tracked_boundary -> "tracked-boundary"
    | Unsupported_boundary -> "unsupported-boundary"
  in
  let[@log_value.debug] classification_name = function
    | Retain_existing -> "retain-existing"
    | Contract_only_exec -> "contract-only-exec"
    | Opaque_model_spec _ -> "opaque-model-spec"
    | Ineligible -> "ineligible"
  in
  let boundary = result_boundary definition result_mode in
  let[@log_value.debug] _result_type =
    Parametric_type.to_string definition.Sst.result_type
  in
  let[@log_value.debug] _surface_contains_mathematical_int =
    Parametric_type.contains_mathematical_int definition.result_type
  in
  [%log.debug "classifying authenticated retained callable result"
    ~provider:(Delator.Field.string provider.unit_name)
    ~path:(Delator.Field.string resolved_path)
    ~stage:(Delator.Field.string "retained-result-classification")
    ~boundary:
      (Delator.Field.string
         ((result_boundary_name [@log_value.debug]) boundary))
    ~result_type:
      (Delator.Field.string (_result_type [@log_value.debug]))
    ~surface_contains_mathematical_int:
      (Delator.Field.bool
         (_surface_contains_mathematical_int [@log_value.debug]))
    ~decision:(Delator.Field.string "started")];
  let result_schema =
    schema_supported ~types ~parametric_adts ~logical_required:false ~boundary
      definition.Sst.result_type
  in
  let decision =
    if not result_schema.supported then
      (Ineligible, "unsupported-result-schema-or-boundary")
    else if not (aggregate_bearing definition.result_type) then
      (Retain_existing, "supported-first-order-result")
    else
      match (definition.mode, definition.body, model) with
      | Sst.Exec, Sst.Checked_exec _, None ->
          (Contract_only_exec, "authenticated-exec-contract")
      | Sst.Spec, Sst.Spec_definition _, Some { domain; result_type }
        when Parametric_type.equal definition.result_type result_type
             &&
             (schema_supported ~types ~parametric_adts ~logical_required:true
                ~boundary definition.result_type)
               .supported
             &&
             (match definition.parameters with
             | [ parameter ] ->
                 let parameter = Sst.require_value_parameter parameter in
                 parameter.pattern.typ = Sst.Aggregate domain
             | _ -> false) ->
          (match model_result_identity parametric_adts result_type with
          | Some identity ->
              ( Opaque_model_spec
                  {
                    closure_type_ids = [];
                    closure_digest =
                      digest
                        (String.concat "\000"
                           ([ "retained-opaque-model-schema-v1";
                              provider.unit_name;
                              provider.interface_digest;
                              resolved_path;
                              binding_uid ]
                           @ identity));
                  },
                "authenticated-opaque-model" )
          | None -> (Ineligible, "missing-model-result-identity"))
      | _, _, None
        when boundary = Ghost_summary
             && result_schema.contains_mathematical_int ->
          (Retain_existing, "authenticated-ghost-mathematical-summary")
      | _ -> (Ineligible, "unsupported-aggregate-result-role")
  in
  let classification = fst decision in
  let[@log_value.debug] reason_class = snd decision in
  [%log.debug "classified authenticated retained callable result"
    ~provider:(Delator.Field.string provider.unit_name)
    ~path:(Delator.Field.string resolved_path)
    ~stage:(Delator.Field.string "retained-result-classification")
    ~boundary:
      (Delator.Field.string
         ((result_boundary_name [@log_value.debug]) boundary))
    ~classification:
      (Delator.Field.string
         ((classification_name [@log_value.debug]) classification))
    ~decision:
      (Delator.Field.string
         (match classification with
         | Ineligible -> "excluded"
         | Retain_existing | Contract_only_exec | Opaque_model_spec _ ->
             "retained"))
    ~reason_class:
      (Delator.Field.string (reason_class [@log_value.debug]))];
  Ok classification
[@@delator.instrument]
[@@delator.level debug]
[@@delator.no_exn_log]
