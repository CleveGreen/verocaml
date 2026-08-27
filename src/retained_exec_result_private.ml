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

let field_is_first_tranche typ field =
  field.Sst.field_mutability = Sst.Immutable_field
  && field.field_modalities.uniqueness_modality = Sst.Preserve_uniqueness
  && field.field_modalities.linearity_modality = Sst.Preserve_linearity
  &&
  match List.assoc_opt field.field_id typ.field_modes with
  | Some (Sst.Exec_instance | Sst.Ghost_instance) -> true
  | Some Sst.Tracked_instance | None -> false

let provider_type types type_id =
  List.find_opt
    (fun typ -> same_type_id typ.definition.Sst.type_id type_id)
    types

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
  | Sst.Unit | Bool | Int | Tuple _ | Aggregate _ | Parameter _ -> None

let schema_supported ~types ~parametric_adts ~logical_required typ =
  let rec visit visiting = function
    | Sst.Unit | Bool | Int | Parameter _ -> true
    | Tuple components ->
        List.for_all (fun (_, typ) -> visit visiting typ) components
    | Aggregate type_id -> visit_aggregate visiting type_id
    | Application (_, arguments) as application ->
        List.for_all (visit visiting) arguments
        && Option.is_some (schema_for_application parametric_adts application)
  and visit_aggregate visiting type_id =
    if List.exists (same_type_id type_id) visiting then true
    else
      match provider_type types type_id with
      | None -> false
      | Some typ ->
          typ.revealed
          && typ.definition.representation = Sst.Revealed
          && ((not logical_required) || typ.logical)
          && List.for_all
               (fun field ->
                 field_is_first_tranche typ field
                 && visit (type_id :: visiting) field.field_type)
               (fields_of_definition typ.definition)
  in
  visit [] typ

let aggregate_bearing typ =
  let rec contains = function
    | Sst.Aggregate _ | Application _ -> true
    | Tuple components -> List.exists (fun (_, typ) -> contains typ) components
    | Unit | Bool | Int | Parameter _ -> false
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
  | Sst.Unit | Bool | Int | Tuple _ | Parameter _ -> None

let classify ~provider ~types ~parametric_adts ~resolved_path ~binding_uid
    ~definition ~model =
  if
    not
      (schema_supported ~types ~parametric_adts ~logical_required:false
         definition.Sst.result_type)
  then
    Ok Ineligible
  else if not (aggregate_bearing definition.result_type) then
    Ok Retain_existing
  else
    match (definition.mode, definition.body, model) with
    | Sst.Exec, Sst.Checked_exec _, None -> Ok Contract_only_exec
    | Sst.Spec, Sst.Spec_definition _, Some { domain; result_type }
      when Parametric_type.equal definition.result_type result_type
           && schema_supported ~types ~parametric_adts ~logical_required:true
                definition.result_type
           &&
           (match definition.parameters with
           | [ parameter ] ->
               let parameter = Sst.require_value_parameter parameter in
               parameter.pattern.typ = Sst.Aggregate domain
           | _ -> false) ->
        (match model_result_identity parametric_adts result_type with
        | Some identity ->
            Ok
              (Opaque_model_spec
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
                 })
        | None -> Ok Ineligible)
    | _ -> Ok Ineligible
