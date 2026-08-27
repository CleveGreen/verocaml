type surface = {
  structure_items : int;
  value_bindings : int;
  expression_metadata : int;
  pattern_metadata : int;
  pattern_modes : int;
  parameter_modes : int;
  identifier_unique_uses : int;
  resolved_pattern_barriers : int;
  resolved_field_barriers : int;
  field_reads : int;
  field_writes : int;
  mutable_local_bindings : int;
  mutable_local_reads : int;
  mutable_local_writes : int;
  resolved_calls : int;
  resolved_call_paths : string list;
}

val probe : Typedtree.structure -> surface

val authenticate_abstraction :
  definition:Sst.type_definition ->
  types:Sst.type_definition list ->
  functions:Sst.function_definition list ->
  Sst.same_cmt_abstraction_evidence ->
  bool

val authenticate_recursive_specification :
  definition:Sst.function_definition -> bool

type issued_rank_domain

type rank_type_identity = {
  rank_type_id : Sst.type_id;
  rank_path : string;
  rank_uid : string;
  rank_span : Diagnostic.span;
}

type rank_positive_child = {
  rank_constructor : Sst.constructor_id;
  rank_constructor_uid : string;
  rank_field : Sst.field_id;
  rank_field_uid : string;
  rank_child_path : int list;
  rank_child_type : Sst.type_id;
  rank_expansion_trace : string list;
}

type rank_ground_witness = {
  rank_ground_constructor : Sst.constructor_id;
  rank_ground_constructor_uid : string;
}

type rank_parameter_classification =
  | Prohibited_negative_use
  | Recursive_dependent_grounding
  | Independently_grounded_construction

type rank_parameter_profile = {
  rank_parameter_index : int;
  rank_parameter_identity : string;
  rank_parameter_classification : rank_parameter_classification;
  rank_parameter_variance : string;
  rank_parameter_occurrence_traces : string list list;
}

type rank_application = {
  rank_application_owner : rank_type_identity;
  rank_application_target : rank_type_identity;
  rank_application_target_profile : string;
  rank_application_substitution : (int * string) list;
  rank_application_trace : string list;
}

type issued_rank_profile

val certify_rank_profiles :
  source_file:string ->
  imports:Cmt_input.import array ->
  Typedtree.structure ->
  (issued_rank_profile list, Diagnostic.t) result

val issued_rank_profiles : Typedtree.structure -> issued_rank_profile list
val rank_profile_id : issued_rank_profile -> string
val rank_profile_snapshot_digest : issued_rank_profile -> string
val rank_profile_identity : issued_rank_profile -> rank_type_identity
val rank_profile_parameters : issued_rank_profile -> rank_parameter_profile list
val rank_profile_applications : issued_rank_profile -> rank_application list
val rank_profile_dependencies : issued_rank_profile -> rank_type_identity list
val rank_profile_ground_traces : issued_rank_profile -> string list list
val rank_profile_independently_grounded : issued_rank_profile -> bool

val authenticate_rank_profile :
  structure:Typedtree.structure -> issued_rank_profile -> bool

val authenticate_rank_application :
  issued_rank_profile -> rank_application -> bool

val authenticate_rank_grounding :
  issued_rank_profile -> string list -> bool

val issued_rank_domains : Sst.program -> issued_rank_domain list
val rank_domain_id : issued_rank_domain -> string
val rank_domain_version : issued_rank_domain -> string
val rank_snapshot_digest : issued_rank_domain -> string
val rank_component : issued_rank_domain -> rank_type_identity list
val rank_positive_children : issued_rank_domain -> rank_positive_child list
val rank_ground_witnesses : issued_rank_domain -> rank_ground_witness list
val rank_immutable : issued_rank_domain -> bool

val lower :
  ?allow_imported_opens:bool ->
  source_file:string ->
  imports:Cmt_input.import array ->
  Typedtree.structure ->
  (Sst.program, Diagnostic.t) result
