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

val classify :
  provider:provider_identity ->
  types:provider_type list ->
  parametric_adts:Parametric_adt.t list ->
  resolved_path:string ->
  binding_uid:string ->
  definition:Sst.function_definition ->
  model:model option ->
  (classification, string) result
